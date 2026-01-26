package io.github.smtp.workers;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.math.BigInteger;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoField;
import java.util.ArrayList;
import java.util.Base64;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ThreadLocalRandom;

import javax.net.ssl.SSLException;
import javax.net.ssl.SSLParameters;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.SSLSocketFactory;

import org.jboss.logging.Logger;

import io.github.smtp.application.Mode;
import io.github.smtp.protocol.SmtpError;
import io.github.smtp.server.ServerMode;

import static io.github.smtp.utils.AppUtils.*;

public class SmtpWorker implements Runnable {
    private final Logger logger = Logger.getLogger(getClass());

    private final ServerMode serverMode;

    @SuppressWarnings("unused")
    private final UUID sessionId;

    private Socket socket;
    private InputStream is;
    private OutputStream os;

    private String clientHostname;
    private String clientAddress;

    @SuppressWarnings("unused")
    private String serverAddress;

    private final String timestamp;

    private final List<String> whiteList = new LinkedList<>();
    private final boolean acceptAllDomains;

    private boolean needAuth = false;
    private boolean isSecure = false;

    public SmtpWorker(
        final Socket socket,
        final ServerMode serverMode,
        final String serverAddress,
        final UUID id,
        final List<String> whiteList
    )
    {
        this.socket = socket;
        this.serverMode = serverMode;
        this.serverAddress = serverAddress;
        this.sessionId = id;
        this.whiteList.addAll(whiteList);

        this.acceptAllDomains = this.whiteList.contains("*");

        this.timestamp = ZonedDateTime
            .now(ZoneId.systemDefault())
            .format(DateTimeFormatter.ofPattern("EEE, dd MMM yyyy HH:mm:ss Z", Locale.US));

        this.isSecure = socket instanceof SSLSocket;

        this.needAuth = List.of(ServerMode.SUBMISSION, ServerMode.SECURE_SUBMISSION).contains(this.serverMode);
    }

    private Mode mode;
    public SmtpWorker setMode(Mode mode) {
        this.mode = mode;
        return this;
    }

    private String hostname;
    public SmtpWorker setHostname(final String hostname)
    {
        this.hostname = hostname;
        return this;
    }

    private String contentFolder;
    public SmtpWorker setContentFolder(final String contentFolder)
    {
        this.contentFolder = contentFolder;
        return this;
    }

    private List<String> forbiddenHostWords = new ArrayList<>();
    private Optional<String> forbiddenHostMessage = Optional.empty();
    public SmtpWorker setForbiddenHostConfig(final List<String> words, final Optional<String> message)
    {
        forbiddenHostWords.clear();
        forbiddenHostWords.addAll(words);

        forbiddenHostMessage = message;

        return this;
    }

    private SSLSocketFactory sslSocketFactory;
    public SmtpWorker setSslSocketFactory(SSLSocketFactory sslSocketFactory)
    {
        this.sslSocketFactory = sslSocketFactory;
        return this;
    }

    private Map<String, String> addressHostCache;
    public SmtpWorker setAddressHostCache(final Map<String, String> addressHostCache)
    {
        this.addressHostCache = addressHostCache;
        return this;
    }

    public void run()
    {
        processRequest();
    }

    private void processRequest() {
        try {
            process();
        } catch (SSLException failure) {
            // ignore
        } catch (IOException failure) {
            logger.warnf("[%s] (processRequest) <%s> %s",
                this.serverMode.name(),
                failure.getClass().getName(),
                failure.getMessage());
        } finally {
            close();
        }
    }

    private void close()
    {
        try {
            logger.trace("--- shutting down connection ---");
            socket.shutdownOutput();
        } catch(IOException failure)
        {
            logger.warn(failure.getMessage());
        }

        try {
            socket.close();
        } catch(IOException failure)
        {
            logger.warn(failure.getMessage());
        }

        logger.debugf("Remote peer: %s --- connection closed", this.clientAddress);
    }

    int commandLength = 0;
    private void process() throws IOException {
        final var remoteInetAddress = socket.getInetAddress();
        final String clientAddress = remoteInetAddress.getHostAddress();

        this.clientAddress = clientAddress;
        this.clientHostname = addressHostCache
            .computeIfAbsent(clientAddress, k -> remoteInetAddress.getCanonicalHostName());

        if(this.clientAddress.equals(this.clientHostname))
        {
            logger.debugf("Remote peer: %s", this.clientAddress);
        } else
        {
            logger.debugf("Remote peer: %s [%s]", this.clientAddress, this.clientHostname);
        }

        this.socket.setSoTimeout(30000);

        if(socket instanceof SSLSocket sslSocket)
        {
            sslSocket.startHandshake();

            this.is = new BufferedInputStream(sslSocket.getInputStream());
            this.os = new BufferedOutputStream(sslSocket.getOutputStream());
        } else
        {
            this.is = socket.getInputStream();
            this.os = socket.getOutputStream();
        }

        startPresentation();
        interact();
    }

    private byte startPresentation() throws IOException {
        final boolean forbiddenHost = this.forbiddenHostWords.stream()
            .filter(this.clientHostname::contains)
            .count() > 0;

        final String presentation = forbiddenHost
            ? String.format("521 %s", forbiddenHostMessage.orElse("Server does not accept mail from you"))
            : String.format("220 %s ESMTP Ready", this.hostname);
        logger.infof("S: %s", presentation);

        writeLine(os, presentation);
        os.flush();

        if(forbiddenHost)
        {
            throw new IOException(String.format("Host <<%s>> not allowed", this.clientHostname));
        }

        return 0;
    }

    private byte interact() throws IOException {
        final ByteArrayOutputStream st = new ByteArrayOutputStream();

        int reader = -1;
        while ((reader = is.read()) != -1) {
            if (reader == '\r') {
                continue;
            }

            if (reader == '\n') {
                if(commandLength > MAX_COMMAND_LENGTH)
                {
                    invalidCommand();
                    continue;
                }

                try {
                    final byte[] raw = st.toByteArray();
                    st.reset();
                    checkStatement(raw);
                } catch (IOException e) {
                    break;
                }

                continue;
            }

            commandLength += 1;
            if(commandLength <= MAX_COMMAND_LENGTH)
            {
                st.write(reader);
            } else if(st.size() > 0)
            {
                st.reset();
            }
        }

        return 0;
    }

    private byte checkStatement(final byte[] raw) throws IOException {
        final String statement = new String(raw, StandardCharsets.UTF_8);

        logger.infof("C: %s", statement);

        if ("QUIT".equalsIgnoreCase(statement))
        {
            quit(); // will throw exception to close
        }

        if ("HELP".equalsIgnoreCase(statement))
        {
            return help();
        }

        if ("NOOP".equalsIgnoreCase(statement))
        {
            return noop();
        }

        if ("RSET".equalsIgnoreCase(statement))
        {
            return rset();
        }

        if("HELO".equalsIgnoreCase(statement))
        {
            return syntaxError();
        }

        if ( matchesStart(statement, "HELO ") )
        {
            return helo(statement);
        }

        if("EHLO".equalsIgnoreCase(statement))
        {
            return ehlo(null);
        }

        if ( matchesStart(statement, "EHLO ") )
        {
            return ehlo(statement);
        }

        if ("STARTTLS".equalsIgnoreCase(statement))
        {
            return startTls();
        }

        if ("VRFY".equalsIgnoreCase(statement))
        {
            return verifyBadSintax();
        }

        if ( matchesStart(statement, "VRFY ") )
        {
            return verify(statement);
        }

        if ("EXPN".equalsIgnoreCase(statement))
        {
            return verifyBadSintax();
        }

        if ( matchesStart(statement, "EXPN ") )
        {
            return expand(statement);
        }

        if( ! ServerMode.SMTP.equals(this.serverMode))
        {
            if ("AUTH LOGIN".equalsIgnoreCase(statement))
            {
                return authLogin();
            }

            if ( matchesStart(statement, "AUTH LOGIN ") )
            {
                return authLogin(statement, raw);
            }

            if ("AUTH PLAIN".equalsIgnoreCase(statement))
            {
                return authPlain("");
            }

            if ( matchesStart(statement, "AUTH PLAIN ") )
            {
                return authPlain(statement.substring(11));
            }
        }

        if ( matchesStart(statement, "MAIL FROM:") )
        {
            return mailFrom(statement);
        }

        if ( matchesStart(statement, "RCPT TO:") )
        {
            return rcptTo(statement);
        }

        if ("DATA".equalsIgnoreCase(statement))
        {
            return data();
        }

        if ("BDAT".equalsIgnoreCase(statement))
        {
            return syntaxError();
        }

        if ( matchesStart(statement, "BDAT ") )
        {
            return binaryData(statement);
        }

        return invalidCommand();
    }

    private byte verifyBadSintax() throws IOException
    {
        logger.infof("S: %s", SmtpError.MAILBOX_MISSING);

        writeLine(os, SmtpError.MAILBOX_MISSING);
        os.flush();

        return 0;
    }

    private byte messageSizeExceeded() throws IOException
    {
        logger.infof("S: %s", SmtpError.MESSAGE_TOO_BIG);

        writeLine(os, SmtpError.MESSAGE_TOO_BIG);
        os.flush();

        return 1;
    }

    @SuppressWarnings("unused")
    private byte unavailable() throws IOException {
        final String message = SmtpError.UNAVAILABLE.withHost(this.hostname);
        logger.infof("S: %s", message);

        writeLine(os, message);
        os.flush();

        return 0;
    }

    private byte invalidCommand() throws IOException {
        logger.infof("S: %s", SmtpError.INVALID_COMMAND);

        writeLine(os, SmtpError.INVALID_COMMAND);
        os.flush();

        return 0;
    }

    private byte syntaxError() throws IOException {
        logger.infof("S: %s", SmtpError.SYNTAX_ERROR);

        writeLine(os, SmtpError.SYNTAX_ERROR);
        os.flush();

        return 0;
    }

    @SuppressWarnings("unused")
    private byte securityPolicy() throws IOException {
        logger.infof("S: %s", SmtpError.SECURITY_POLICY);

        writeLine(os, SmtpError.SECURITY_POLICY);
        os.flush();

        return 0;
    }
    
    private byte help() throws IOException {
        final StringBuilder response = new StringBuilder();

        response.append("211 2.0.0 EHLO HELO NOOP RSET VRFY EXPN AUTH MAIL RCPT HELP DATA BDAT");
        logger.infof("S: %s", response);

        writeLine(os, response);
        os.flush();

        return 0;
    }
    /*
     * Reply Codes in Numeric Order
     * https://www.rfc-editor.org/rfc/rfc5321#section-4.2.3
     */

    private String heloHost;

    private byte helo(final String statement) throws IOException {
        this.heloHost = statement.substring(5);

        final String response = "250 " + this.hostname + " greets " + this.heloHost;
        logger.infof("S: %s", response);

        writeLine(os, response);
        os.flush();

        return 0;
    }

    private byte ehlo(final String statement) throws IOException
    {
        final String heloHost = this.clientAddress.equals(this.clientHostname)
            ? String.format("[%s]", this.clientAddress)
            : this.clientHostname;

        this.heloHost = statement != null ? statement.substring(5) : heloHost;

        final List<String> responses = new ArrayList<>();

        responses.add("250-" + this.hostname + " greets " + this.heloHost);
        responses.add("250-SIZE " + MAX_MESSAGE_SIZE);
        responses.add("250-ENHANCEDSTATUSCODES");
        responses.add("250-PIPELINING");

        if( this.needAuth && this.isSecure )
        {
            responses.add("250-AUTH LOGIN PLAIN");
            responses.add("250-AUTH=LOGIN PLAIN");
        }

        if( ! ServerMode.SECURE_SUBMISSION.equals(this.serverMode) && ! this.isSecure )
        {
            responses.add("250-STARTTLS");
        }

        responses.add("250-HELP");
        responses.add("250-SMTPUTF8");
        responses.add("250-8BITMIME");
        responses.add("250-CHUNKING");
        responses.add("250 BINARYMIME");

        final var accumulator = new StringBuilder();
        for(final String line: responses)
        {
            logger.infof("S: %s", line);
            accumulator.append(line).append("\r\n");
        }

        os.write(asciiraw(accumulator));
        os.flush();

        return 0;
    }

    // https://docs.oracle.com/cd/E54932_01/doc.705/e54936/cssg_create_ssl_cert.htm#CSVSG180

    private byte startTls() throws IOException
    {
        if(this.heloHost == null) {
            return introductionMissing();
        }

        final String message = this.isSecure 
            ? SmtpError.TLS_ALREADY_ACTIVE.toString()
            : "220 2.0.0 Ready to start TLS";

        // final String response = ""220 Ready to start TLS";
        logger.infof("S: %s", message);

        writeLine(os, message);
        os.flush();

        return ! this.isSecure ? startSecureChain() : 0;
    }

    private byte startSecureChain() throws IOException
    {
        final SSLSocket sslSocket = (SSLSocket) sslSocketFactory
            .createSocket(
                this.socket, 
                this.socket.getInetAddress().getHostAddress(),
                this.socket.getPort(),
                true
            );

        final SSLParameters params = sslSocket.getSSLParameters();
        params.setApplicationProtocols(new String[] {"smtp"});
        params.setProtocols(new String[] {"TLSv1", "TLSv1.1", "TLSv1.2", "TLSv1.3"});
        params.setUseCipherSuitesOrder(true);

        sslSocket.setSSLParameters(params);

        sslSocket.setUseClientMode(false);

        sslSocket.startHandshake();

        this.socket = sslSocket;
        this.is = new BufferedInputStream(sslSocket.getInputStream());
        this.os = new BufferedOutputStream(sslSocket.getOutputStream());

        this.isSecure = true;

        resetState();

        logger.debug(">>> Secure channel ON <<<");

        return 0;
    }

    private boolean authenticated = false;
    private String username;
    private String password;

    static final String LOCAL_USERNAME = "admin@example.com";
    static final String LOCAL_PASSWORD = "myp@77";

    private byte notSecure() throws IOException
    {
        logger.infof("S: %s", SmtpError.NEED_STARTTLS);

        writeLine(os, SmtpError.NEED_STARTTLS);
        os.flush();

        return 1;
    }

    private byte resetAuthentication() throws IOException
    {
        this.authenticated = false;
        this.username = null;
        this.password = null;

        logger.info("C: *");
        logger.infof("S: %s", SmtpError.AUTHENTICATION_ABORTED);

        writeLine(os, SmtpError.AUTHENTICATION_ABORTED);
        os.flush();

        return 0;
    }

    private byte cannotDecodeResponse() throws IOException
    {
        this.authenticated = false;
        this.username = null;
        this.password = null;

        logger.infof("S: %s", SmtpError.CANNOT_DECODE_RESPONSE);

        writeLine(os, SmtpError.CANNOT_DECODE_RESPONSE);
        os.flush();

        return 0;
    }

    /*
     * SMTP Service Extension for Authentication
     * https://datatracker.ietf.org/doc/html/rfc4954#section-6
     * 
     * The following error codes may be used to indicate various success or
     * failure conditions. Servers that return enhanced status codes
     * SHOULD use the enhanced codes suggested here.
     */

    private byte authLogin() throws IOException
    {
        if( ! this.isSecure )
        {
            return notSecure();
        }

        final String usernameLabel = "Username:"; // base64: VXNlcm5hbWU6

        logger.info("S: Awaiting for " + usernameLabel);

        final byte[] data = joinEndLine(
            asciiraw("334 "),
            Base64.getEncoder().encode(asciiraw(usernameLabel))
        );

        os.write(data);
        os.flush();

        final String username = getContent();

        return validateAuthLoginCredential(username);
    }

    private byte authLogin(String statement, byte[] raw) throws IOException
    {
        if( ! this.isSecure )
        {
            return notSecure();
        }

        if (username.isBlank())
        {
            return authLogin();
        }

        if (username.isBlank())
        {
            logger.infof("S: %s", SmtpError.INVALID_CREDENTIALS);

            writeLine(os, SmtpError.INVALID_CREDENTIALS);
            os.flush();

            return 0;
        }

        return validateAuthLoginCredential(username);
    }

    private byte validateAuthLoginCredential(String username) throws IOException
    {
        if("*".equals(username))
        {
            return resetAuthentication();
        }

        final byte[] rawuser;
        try
        {
            rawuser = Base64.getDecoder().decode(username);
        } catch(IllegalArgumentException failure)
        {
            return cannotDecodeResponse();
        }

        this.username = new String(rawuser, StandardCharsets.US_ASCII);
        logger.info("C: Username: " + this.username);

        final String passwordLabel = "Password:"; // base64: UGFzc3dvcmQ6
        logger.info("S: Awaiting for " + passwordLabel);

        final byte[] data = joinEndLine(
            asciiraw("334 "),
            Base64.getEncoder().encode(asciiraw(passwordLabel))
        );

        os.write(data);
        os.flush();

        final String password = getContent();

        if("*".equals(password))
        {
            return resetAuthentication();
        }

        final byte[] rawpass;
        try
        {
            rawpass = Base64.getDecoder().decode(password);
        } catch(IllegalArgumentException failure)
        {
            return cannotDecodeResponse();
        }

        final StringBuilder response = new StringBuilder();

        this.password = new String(rawpass, StandardCharsets.US_ASCII);
        logger.info("C: Password: " + this.password);

        if(Mode.RELAXED.equals(this.mode))
        {
            this.authenticated = true;
            response.append(AUTENTHICATION_SUCEEDED);
        } else if (LOCAL_USERNAME.equals(this.username) && LOCAL_PASSWORD.equals(this.password))
        {
            this.authenticated = true;
            response.append(AUTENTHICATION_SUCEEDED);
        } else
        {
            this.username = null;
            this.password = null;
            response.append(SmtpError.INVALID_CREDENTIALS);
        }

        logger.infof("S: %s", response);

        writeLine(os, response);
        os.flush();

        return 0;
    }

    private String getContent() throws IOException {
        final ByteArrayOutputStream data = new ByteArrayOutputStream();

        int[] control = { -1, -1 };
        int reader = -1;
        while ((reader = is.read()) != -1) {
            control[0] = control[1];
            control[1] = reader;

            if (reader == '\r') {
                continue;
            }

            if (control[0] == '\r' && control[1] == '\n') {
                break;
            }

            data.write(reader);
        }

        return new String(data.toByteArray(), StandardCharsets.US_ASCII);
    }

    private byte authPlain(String credential) throws IOException
    {
        if( ! this.isSecure )
        {
            return notSecure();
        }

        if (credential.isEmpty())
        {
            return authPlainTransition();
        }

        return authPlainValidation(credential);
    }

    private byte authPlainTransition() throws IOException
    {
        final String response = "334 "; // deve ser assim obrigatoriamente
        logger.infof("S: %s]", response);

        writeLine(os, response);
        os.flush();

        final String credential = getContent();
        logger.infof("C: %s", credential);

        return authPlainValidation(credential);
    }

    private byte authPlainValidation(String credential) throws IOException
    {
        if("*".equals(credential))
        {
            return resetAuthentication();
        }

        final StringBuilder response = new StringBuilder();

        if (credential.trim().isEmpty()) {
            response.append(SmtpError.INVALID_CREDENTIALS);
        } else {
            checkEncodedCredential(credential);
            response.append(this.authenticated
                ? AUTENTHICATION_SUCEEDED
                : SmtpError.INVALID_CREDENTIALS.toString()
            );
        }

        logger.infof("S: %s", response);

        writeLine(os, response);
        os.flush();

        return 0;
    }

    private void checkEncodedCredential(final String credential)
    {
        String decodedCredential;
        try
        {
            decodedCredential = new String(
                Base64.getDecoder().decode(credential),
                StandardCharsets.US_ASCII);
        } catch(IllegalArgumentException failure)
        {
            decodedCredential = "";
        }

        final String[] credentialParts = decodedCredential.split("\0");
        if(credentialParts.length != 3)
        {
            return;
        }

        if (LOCAL_USERNAME.equals(credentialParts[1]) && LOCAL_PASSWORD.equals(credentialParts[2]))
        {
            this.username = credentialParts[1];
            this.password = credentialParts[2];
            this.authenticated = true;

        } else if (Mode.RELAXED.equals(this.mode))
        {
            this.username = credentialParts[1];
            this.password = credentialParts[2];
            this.authenticated = true;

        }
    }

    private byte verify(final String statement) throws IOException {
        final List<String> responses = new ArrayList<>();

        final String mailbox = statement.substring(statement.indexOf(' ') + 1).trim();

        if (MAILING_LIST_EXAMPLE.is(mailbox)) {
            responses.add(SmtpError.INVALID_MAILING_LIST_AS_USER.toString());
        } else if ((JOHN_EXAMPLE.getUser()).equals(mailbox) || JOHN_EXAMPLE.is(mailbox)) {
            responses.add("250 " + JOHN_EXAMPLE.getFullEmail());
        } else if ((JANE_EXAMPLE.getUser()).equals(mailbox) || JANE_EXAMPLE.is(mailbox)) {
            responses.add("250 " + JANE_EXAMPLE.getFullEmail());
        } else if ("doe".equals(mailbox)) {
            responses.add("553-Ambiguous; Possibilities are");
            responses.add("553-" + JOHN_EXAMPLE.getFullEmail());
            responses.add("553 " + JANE_EXAMPLE.getFullEmail());
        } else {
            responses.add("252-2.1.0 Unable to verify user");
            responses.add("252 2.1.0 send some mail, I'll try my best");
        }

        final var accumulator = new StringBuilder();
        for(final String line: responses)
        {
            logger.infof("S: %s", line);
            accumulator.append(line).append("\r\n");
        }

        os.write(asciiraw(accumulator));
        os.flush();

        return 0;
    }

    private byte expand(final String statement) throws IOException {
        final List<String> responses = new ArrayList<>();

        final String mailbox = statement.substring(statement.indexOf(' ') + 1).trim();

        if (MAILING_LIST_EXAMPLE.is(mailbox) || MAILING_LIST_EXAMPLE.getUser().equals(mailbox)) {
            responses.add("250-" + JOHN_EXAMPLE.getFullEmail());
            responses.add("250 " + JANE_EXAMPLE.getFullEmail());
        } else if ((JOHN_EXAMPLE.getUser()).equals(mailbox) || JOHN_EXAMPLE.is(mailbox)) {
            responses.add(SmtpError.INVALID_USER_AS_MAILING_LIST.toString());
        } else if ((JANE_EXAMPLE.getUser()).equals(mailbox) || JANE_EXAMPLE.is(mailbox)) {
            responses.add(SmtpError.INVALID_USER_AS_MAILING_LIST.toString());
        } else {
            responses.add("252 2.1.0 Unable to verify mailbox for mailing list");
        }

        final var accumulator = new StringBuilder();
        for(final String line: responses)
        {
            logger.infof("S: %s", line);
            accumulator.append(line).append("\r\n");
        }

        os.write(asciiraw(accumulator));
        os.flush();

        return 0;
    }

    private Boolean fromHost = null;
    private Mailbox sender = null;
    private Map<String, Object> mailParams = new LinkedHashMap<>();

    private byte mailFrom(final String statement) throws IOException {
        if(this.heloHost == null) {
            return introductionMissing();
        }

        final String mailbox = statement.substring(10);

        String name = "";
        String email = mailbox;

        if (mailbox.contains(" ")) {
            name = mailbox.substring(0, mailbox.indexOf('<'));
            email = mailbox.substring(mailbox.indexOf('<'), mailbox.indexOf('>')+1);
        }
        email = "<>".equals(email) ? "@" : email.replaceAll("[<>]", "");

        final String user = email.substring(0, email.indexOf('@'));
        final String domain = email.substring(email.indexOf('@') + 1).toLowerCase();
        final Mailbox sender = new Mailbox(name, user, domain);

        parseMailParams(statement);

        this.fromHost = this.whiteList
            .stream()
            .filter( h -> h.equalsIgnoreCase(sender.getDomain()) )
            .findFirst()
            .isPresent();

        final StringBuilder response = new StringBuilder();

        if ( (this.needAuth || Boolean.TRUE.equals(fromHost)) && !authenticated ) {
            this.fromHost = null;
            response.append(SmtpError.AUTHENTICATION_REQUIRED);
        } else {            
            this.recipients.clear();
            this.toHost = null;

            final BigInteger messageSize = getMessageSize();
            if(messageSize.compareTo(BigInteger.valueOf(MAX_MESSAGE_SIZE)) > 0)
            {
                this.mailParams.clear();
                this.fromHost = null;
                response.append(SmtpError.MESSAGE_TOO_BIG);
            } else 
            {
                this.sender = sender;
                response.append(String.format("250 2.1.0 <%s>: Originator OK", this.sender.getEmail()));
            }
        }

        logger.infof("S: %s", response);

        final byte[] output = utf8raw(response.toString()+"\r\n");

        os.write(output);
        os.flush();

        return 0;
    }

    private boolean checkDomain(final String whiteDomain, final String domain)
    {
        if(acceptAllDomains)
        {
            return true;
        }

        if(whiteDomain.startsWith("*"))
        {
            final String subdomain = whiteDomain.substring(1);
            return matchesEnd(domain, subdomain);
        }

        return whiteDomain.equalsIgnoreCase(domain);
    }

    private void parseMailParams(final String statement)
    {
        int p = statement.indexOf('>');
        if(p < 0)
        {
            return;
        }

        this.mailParams.clear();
        final String[] tokens = statement.substring(p + 1).split(" ");
        for(String token: tokens)
        {
            if(token.isBlank())
            {
                continue;
            }

            final String[] params = token.split("=");
            final String key = params[0];
            final Object value = params.length > 1 ? params[1] : Boolean.TRUE;

            this.mailParams.put(key.toUpperCase(), value);
            logger.tracef("S: (extension detected: %s = %s)", key, value);
        }
    }

    private BigInteger getMessageSize()
    {
        return Optional
            .ofNullable(this.mailParams.get("SIZE"))
            .map(String::valueOf)
            .map(BigInteger::new)
            .orElseGet(()->BigInteger.valueOf(MAX_MESSAGE_SIZE));
    }

    private Boolean toHost = null;
    private List<Mailbox> recipients = new LinkedList<>();

    private byte rcptTo(final String statement) throws IOException {
        if(this.heloHost == null)
        {
            return introductionMissing();
        }

        final String mailbox = statement.substring(statement.indexOf('<'));

        String name = "";
        String email = mailbox;

        if (mailbox.contains(" ")) {
            name = mailbox.substring(0, mailbox.indexOf('<'));
            email = mailbox.substring(mailbox.indexOf('<'), mailbox.indexOf('>')+1);
        }
        email = email.replaceAll("[<>]", "");

        final String user = email.substring(0, email.indexOf('@'));
        final String domain = email.substring(email.indexOf('@') + 1).toLowerCase();

        final Mailbox recipient = new Mailbox(name, user, domain);

        this.toHost = this.whiteList
            .stream()
            .filter( host -> checkDomain(host, recipient.getDomain()) )
            .findFirst()
            .isPresent();

        final List<String> responses = new ArrayList<>();

        if (this.sender == null) {
            responses.add(SmtpError.SENDER_MISSING.toString());
        } else if ( Boolean.FALSE.equals(fromHost) && Boolean.FALSE.equals(toHost) && ! this.authenticated ) {
            toHost = null;
            responses.add(SmtpError.RELAY_FORBIDDEN.toString());
        } else {
            this.recipients.add(recipient);
            responses.add(String.format("250 2.1.0 <%s>: Recipient OK", recipient.getEmail()));
        }

        final var accumulator = new StringBuilder();
        for(final String line: responses)
        {
            logger.infof("S: %s", line);
            accumulator.append(line).append("\r\n");
        }

        final byte[] output = utf8raw(accumulator);
        os.write(output);
        os.flush();

        return 0;
    }

    private byte noop() throws IOException {
        final String response = "211 2.0.0 OK";
        logger.infof("S: %s", response);

        writeLine(os, response);
        os.flush();

        return 0;
    }

    // https://stackoverflow.com/questions/25710599/content-transfer-encoding-7bit-or-8-bit

    private byte data() throws IOException {
        final String bodyContent = Optional
            .ofNullable(this.mailParams.get(BODY))
            .map(String.class::cast)
            .orElse(null);

        if(BINARYMIME.equalsIgnoreCase(bodyContent))
        {
            return invalidCommand();
        }

        if(this.binaryDataInProgress.size() > 0)
        {
            return invalidCommand();
        }

        if(this.heloHost == null) {
            return introductionMissing();
        }

        boolean dataInProgress = false;

        final StringBuilder response = new StringBuilder();

        if (this.fromHost == null && this.toHost == null) {
            response.append(SmtpError.RECIPIENTS_MISSING);
        } else if (this.fromHost == null) {
            response.append(SmtpError.SENDER_MISSING);
        } else if (this.toHost == null) {
            response.append(SmtpError.DESTINATION_MISSING);
        } else {
            dataInProgress = true;
            final Object bodyMime = this.mailParams.get(BODY);
            response
                .append("354 I am ready, send ")
                .append(bodyMime != null ? bodyMime + " " : "")
                .append("message, ending in <CRLF>.<CRLF>");
        }

        logger.infof("S: %s", response);

        writeLine(os, response);
        os.flush();

        if (dataInProgress) {
            consumeData();
        }

        return 0;
    }

    private byte binaryData(final String statement) throws IOException {
        if(this.heloHost == null) {
            return introductionMissing();
        }

        BigInteger chunkSize = BigInteger.ZERO;

        final String[] args = statement
            .replaceAll("\\s+$", "")
            .replaceAll("\\s+", " ").split(" ");

        if( args.length < 2 )
        {
            return syntaxError();
        }

        try
        {
            chunkSize = new BigInteger(args[1]);
        } catch(NumberFormatException failure)
        {
            logger.warn(failure.getMessage());
            return syntaxError();
        }

        if(chunkSize.compareTo(BigInteger.valueOf(MAX_MESSAGE_SIZE)) > 0)
        {
            return messageSizeExceeded();
        }

        final boolean lastChunk = args.length > 2 && LAST.equalsIgnoreCase(args[2]);

        boolean dataInProgress = false;

        final StringBuilder response = new StringBuilder();

        if (this.fromHost == null && this.toHost == null) {
            response.append(SmtpError.RECIPIENTS_MISSING);
        } else if (this.fromHost == null) {
            response.append(SmtpError.SENDER_MISSING);
        } else if (this.toHost == null) {
            response.append(SmtpError.DESTINATION_MISSING);
        } else {
            if( ! lastChunk )
            {
                response.append("250 2.0.0 ").append(chunkSize).append(" bytes received");
            }
            dataInProgress = true;
        }

        if (dataInProgress) {
            consumeBinaryData(chunkSize);

            if(lastChunk)
            {
                final int[] queueId = new int[1];
                dataReceived(binaryDataInProgress, queueId);
                resetState();

                response.append("250 2.0.0 OK message queued id ").append(queueId[0]);
            }
        }

        logger.infof("S: %s", response);

        writeLine(os, response);
        os.flush();

        return 0;
    }

    private void resetState()
    {
        this.sender = null;
        this.fromHost = null;
        this.toHost = null;

        this.mailParams.clear();
        this.recipients.clear();
        this.binaryDataInProgress.reset();
    }

    private byte rset() throws IOException {
        resetState();

        final String response = "250 2.0.0 OK";

        logger.infof("S: %s", response);

        writeLine(os, response);
        os.flush();

        return 0;
    }

    private byte quit() throws IOException {
        final String response = "221 2.0.0 " + this.hostname + " Service closing transmission channel";

        logger.infof("S: %s", response);

        writeLine(os, response);
        os.flush();

        throw new IOException("Connection closed by client");
    }

    private byte introductionMissing() throws IOException {
        logger.infof("S: %s", SmtpError.INTRODUCTION_MISSING);

        writeLine(os, SmtpError.INTRODUCTION_MISSING);
        os.flush();

        return 0;
    }

    private byte consumeData() throws IOException {
        final ByteArrayOutputStream data = new ByteArrayOutputStream();

        int control0 = 0;
        int control1 = 0;
        int control2 = 0;
        int control3 = 0;
        int control4 = 0;

        int reader = -1;

        boolean safeData = true;

        while ((reader = is.read()) != -1) {
            if(safeData && data.size() > MAX_MESSAGE_SIZE)
            {
                data.reset();
                safeData = false;
            }

            control0 = control1;
            control1 = control2;
            control2 = control3;
            control3 = control4;
            control4 = reader;

            if(     control0 == '\r'
                &&  control1 == '\n'
                &&  control2 == '.'
                &&  control3 == '\r'
                &&  control4 == '\n'
            )
            {
                if(safeData)
                {
                    final int[] queueId = new int[1];
                    dataReceived(data, queueId);
                    resetState();

                    final String response = "250 2.0.0 OK Message queued id " + queueId[0];
                    logger.infof("S: %s", response);

                    writeLine(os, response);
                    os.flush();

                    return 0;
                }

                return messageSizeExceeded();
            }

            if(!safeData)
            {
                continue;
            }

            if(     control1 == '\r'
                &&  control2 == '\n'
                &&  control3 == '.'
                &&  control4 == '\r'
            )
            {
                continue;
            }

            if(     control2 == '\r'
                &&  control3 == '\n'
                &&  control4 == '.'
            )
            {
                continue;
            }

            if(     control3 == '\r'
                &&  control4 == '\n'
            )
            {
                continue;
            }

            if(control4 == '\r')
            {
                if(control3 == '\r')
                {
                    data.write(control3);
                    control3 = 0;
                }

                if(control2 == '\r' && control3 == '\n')
                {
                    data.write(control2);
                    control2 = 0;

                    data.write(control3);
                    control3 = 0;
                }

                continue;
            }

            if(     control2 == '\r'
                &&  control3 == '\n'
            )
            {
                data.write(control2);
                control2 = 0;

                data.write(control3);
                control3 = 0;
            }

            if(     control1 == '\r'
                &&  control2 == '\n'
                &&  control3 == '.'
            )
            {
                data.write(control1);
                control1 = 0;

                data.write(control2);
                control2 = 0;

                control3 = 0;
            }

            if(     control0 == '\r'
                &&  control1 == '\n'
                &&  control2 == '.'
                &&  control3 == '\r'
            )
            {
                data.write(control0);
                control0 = 0;

                data.write(control1);
                control1 = 0;

                control2 = 0;

                data.write(control3);
                control3 = 0;
            }

            data.write(control4);
            control4 = 0;
        }

        return 1;
    }

    static final int CHUNK_SIZE = 1024;

    final ByteArrayOutputStream binaryDataInProgress = new ByteArrayOutputStream();

    private byte consumeBinaryData(final BigInteger chunkSize) throws IOException
    {
        int remaining = chunkSize.intValue();

        while(remaining > 0)
        {
            final byte[] chunk = new byte[Math.min(remaining, CHUNK_SIZE)];
            int reader = is.read(chunk);
            if(reader == -1)
            {
                break;
            }

            binaryDataInProgress.write(chunk, 0, reader);
            remaining -= reader;
        }

        return 0;
    }

    private byte dataReceived(final ByteArrayOutputStream rawData, final int[] queueId) throws IOException {
        // Queuing only if this server is a relay, otherwise (final destination), persist data

        final var currentTime = LocalDateTime.now();

        final long seconds = currentTime.atZone(ZoneOffset.UTC).toInstant().getEpochSecond();
        final long nano = currentTime.get(ChronoField.NANO_OF_SECOND);
        final int random = ThreadLocalRandom.current().nextInt(10000);

        // Formato: 1700000000.N2837462.R1234.meuserver.com
        final String hash = String.format("%d.N%d.R%04d.%s", seconds, nano, random, "recipient-domain");

        queueId[0] = ThreadLocalRandom.current().nextInt(100000, 1000000);

        final File file = new File(this.contentFolder, hash + ".eml");

        final String receivedHeader = "Received";

        final StringBuilder received = new StringBuilder();
        received.append(receivedHeader);
        received.append(": from: ");
        received.append(this.heloHost);
        received.append(" (");

        if( ! this.clientAddress.equals(this.clientHostname) )
        {
            received.append(this.clientHostname).append(' ');
        }

        received.append('[').append(this.clientAddress).append(']').append(')');
        received.append("\r\n").append(" ".repeat(receivedHeader.length()));
        received.append("by ").append(this.hostname).append(" (Smtp Service)");
        received.append(" with ESMTP");
        if( ! ServerMode.SMTP.equals(serverMode) )
        {
            received.append("S");
            if(this.authenticated)
            {
                received.append("A");
            }
        }

        received.append(" id ").append(queueId[0]);
        received.append("\r\n").append(" ".repeat(receivedHeader.length()));
        received.append("for <").append(this.sender.getEmail()).append(">;");
        received.append(" ").append(this.timestamp);
        received.append("\r\n");

        final byte[] receivedFrom = asciiraw(received);

        try (OutputStream outData = new FileOutputStream(file)) {
            outData.write(receivedFrom);
            rawData.writeTo(outData);
            outData.flush();
        } catch (IOException e) { /***/ }

        logger.debugf("--- Data hash: %s ---", hash);

        return 0;
    }

    private void writeLine(final OutputStream out, final Object content) throws IOException
    {
        final byte[] raw = joinEndLine(asciiraw(content.toString()));

        out.write(raw);
    }

    static class Mailbox {
        private final String fullname;
        private final String user;
        private final String domain;

        Mailbox(final String fullname, final String user, final String domain) {
            this.fullname = fullname;
            this.user = user;
            this.domain = domain;
        }

        Mailbox(final String user, final String domain) {
            this.fullname = "";
            this.user = user;
            this.domain = domain;
        }

        Mailbox(final String email) {
            this.fullname = "";
            this.user = email.substring(0, email.indexOf('@'));
            this.domain = email.substring(email.indexOf('@') + 1);
        }

        public String getFullname() {
            return fullname;
        }

        public String getUser() {
            return user;
        }

        public String getDomain() {
            return domain;
        }

        public String getEmail() {
            return user.isBlank() && domain.isBlank() ? "" : (user + "@" + domain);
        }

        public String getFullEmail() {
            return this.fullname + " <" + this.getEmail() + ">";
        }

        public boolean is(final String email) {
            return getEmail().equals(email);
        }
    }

    static final long MAX_MESSAGE_SIZE = 2 * 1024 * 1024;
    static final int MAX_COMMAND_LENGTH = 2000;

    static final Mailbox JOHN_EXAMPLE = new Mailbox("John Doe", "john.doe", "example.com");
    static final Mailbox JANE_EXAMPLE = new Mailbox("Jane Doe", "jane.doe", "example.com");
    static final Mailbox MAILING_LIST_EXAMPLE = new Mailbox("mailing@example.com");

    static final String BODY = "BODY";
    static final String BINARYMIME = "BINARYMIME";
    static final String LAST = "LAST";

    static final String AUTENTHICATION_SUCEEDED = "235 2.7.0 Authentication Succeeded";
}
