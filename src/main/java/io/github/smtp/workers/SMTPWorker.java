package io.github.smtp.workers;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Base64;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.UUID;
import org.jboss.logging.Logger;

import io.github.smtp.protocol.SmtpError;

public class SMTPWorker implements Runnable {
    private final Logger logger = Logger.getLogger(getClass());

    @SuppressWarnings("unused")
    private final UUID sessionId;

    private final Socket socket;
    private InputStream is;
    private OutputStream os;

    private String clientHost;
    private String clientAddress;

    private String serverAddress;
    
    private final String timestamp;
    
    private final List<String> whiteList = new LinkedList<>();
    
    public SMTPWorker(
        final Socket socket,
        final String serverAddress,
        final UUID id,
        final List<String> whiteList
    )
    {
        this.socket = socket;
        this.serverAddress = serverAddress;
        this.sessionId = id;
        this.whiteList.addAll(whiteList);

        this.timestamp = ZonedDateTime
            .now(ZoneId.systemDefault())
            .format(DateTimeFormatter.ofPattern("EEE, dd MMM yyyy HH:mm:ss Z", Locale.US));

        final var remoteInetAddress = socket.getInetAddress();
        this.clientHost = remoteInetAddress.getHostAddress();
        this.clientAddress = remoteInetAddress.getHostName();
    }

    private String hostname;
    public SMTPWorker setHostname(final String hostname)
    {
        this.hostname = hostname;
        return this;
    }

    private String contentFolder;
    public SMTPWorker setContentFolder(final String contentFolder)
    {
        this.contentFolder = contentFolder;
        return this;
    }

    public void run()
    {        
        processRequest();
    }

    private void processRequest() {
        try {
            process();
        } catch (IOException failure) {
            logger.warn(failure.getMessage());
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

    private void process() throws IOException {
        if(this.clientAddress.equals(this.clientHost))
        {
            logger.debugf("Remote peer: %s", this.clientAddress);
        } else
        {
            logger.debugf("Remote peer: %s [%s]", this.clientAddress, this.clientHost);
        }

        socket.setSoTimeout(30000);

        is = socket.getInputStream();
        os = socket.getOutputStream();

        startPresentation();
        interact();
    }

    private byte startPresentation() throws IOException {
        final String presentation = "220 " + this.hostname + " ESMTP Ready";
        logger.debug(presentation);

        writeLine(os, presentation);
        os.flush();

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
                try {
                    checkStatement(st.toByteArray());
                } catch (IOException e) {
                    break;
                }
                st.reset();
            } else {
                st.write(reader);
            }
        }

        return 0;
    }

    private byte checkStatement(final byte[] raw) throws IOException {
        final String statement = new String(raw, StandardCharsets.US_ASCII);

        logger.infof("C: %s", statement);

        if ("QUIT".equalsIgnoreCase(statement)) {
            quit(); // will throw exception to close
        }

        if ("HELP".equalsIgnoreCase(statement)) {
            return help();
        }

        if (statement.regionMatches(true, 0, "HELO ", 0, 5)) {
            return helo(statement, raw);
        }

        if (statement.regionMatches(true, 0, "EHLO ", 0, 5)) {
            return ehlo(statement, raw);
        }

        if ("STARTTLS".equalsIgnoreCase(statement)) {
            return startTls();
        }

        if ("VRFY".equalsIgnoreCase(statement)) {
            return verifyBadSintax();
        }

        if (statement.regionMatches(true, 0, "VRFY ", 0, 5)) {
            return verify(statement, raw);
        }

        if ("EXPN".equalsIgnoreCase(statement)) {
            return verifyBadSintax();
        }

        if (statement.regionMatches(true, 0, "EXPN ", 0, 5)) {
            return expand(statement, raw);
        }

        if ("AUTH LOGIN".equalsIgnoreCase(statement)) {
            return authLogin();
        }

        if (statement.regionMatches(true, 0, "AUTH LOGIN ", 0, 11)) {
            return authLogin(statement, raw);
        }

        if ("AUTH PLAIN".equalsIgnoreCase(statement)) {
            return authPlain(statement, raw);
        }

        if (statement.regionMatches(true, 0, "AUTH PLAIN ", 0, 11)) {
            return authPlain(statement, raw);
        }

        if (statement.toUpperCase().startsWith("MAIL FROM:")) {
            return mailFrom(statement, raw);
        }

        if (statement.toUpperCase().startsWith("RCPT TO:")) {
            return rcptTo(statement, raw);
        }

        if ("NOOP".equalsIgnoreCase(statement)) {
            return noop();
        }

        if ("DATA".equalsIgnoreCase(statement)) {
            return data();
        }

        if ("RSET".equalsIgnoreCase(statement)) {
            return rset();
        }

        return invalidCommand();
    }

    private byte help() throws IOException {
        final StringBuilder response = new StringBuilder();

        response.append("211 2.0.0 EHLO HELO NOOP RSET VRFY EXPN AUTH MAIL RCPT DATA HELP");
        logger.infof("S: %s", response);

        writeLine(os, response);
        os.flush();

        return 0;
    }
    /*
     * Reply Codes in Numeric Order
     * https://www.rfc-editor.org/rfc/rfc5321#section-4.2.3
     */

    private String introductionHost;

    private byte checkIntroduction(final String host)
    {
        this.introductionHost = host;
        if( host.startsWith("[") && host.endsWith("]") )
        {
            // Validate client IP
            this.introductionHost = host.substring(1, host.length() - 1);
        }

        if( this.serverAddress.equals(this.introductionHost)
                || this.hostname.equals(this.introductionHost)
                || "127.0.0.1".equals(this.introductionHost))
        {
            return 1;
        }

        return 0;
    }

    private byte helo(final String statement, final byte[] raw) throws IOException {
        final String host = statement.substring(5);
        final byte q = checkIntroduction(host);

        if(q == 1)
        {
            return securityPolicy();
        }

        final String response = "250 " + this.hostname + " greets " + this.introductionHost;
        logger.infof("S: %s", response);

        writeLine(os, response);
        os.flush();

        return 0;
    }

    private byte ehlo(final String statement, final byte[] raw) throws IOException {
        final String host = statement.substring(5);
        final byte q = checkIntroduction(host);

        if(q == 1)
        {
            return securityPolicy();
        }

        final List<String> responses = new ArrayList<>();

        responses.add("250-" + this.hostname + " greets " + this.introductionHost);
        responses.add("250-HELP");
        responses.add("250-AUTH LOGIN PLAIN");
        responses.add("250-AUTH=LOGIN PLAIN");
        responses.add("250-ENHANCEDSTATUSCODES");
        responses.add("250 8BITMIME");
        //responses.add("250 BINARYMIME");
        // responses.add("250 CHUNKING");

        for(final String line: responses)
        {
            logger.infof("S: %s", line);
        }

        for(final String line: responses)
        {
            writeLine(os, line);
        }

        os.flush();

        return 0;
    }

    // https://docs.oracle.com/cd/E54932_01/doc.705/e54936/cssg_create_ssl_cert.htm#CSVSG180

    private byte startTls() throws IOException {
        // final String response = ""220 Ready to start TLS";
        logger.tracef("S: %s", SmtpError.TLS_NOT_AVAILABLE);

        writeLine(os, SmtpError.TLS_NOT_AVAILABLE);
        os.flush();

        return 0;
    }

    private boolean authenticated = false;
    private String username;
    private String password;

    static final String LOCAL_USERNAME = "admin@example.com";
    static final String LOCAL_PASSWORD = "myp@77";

    /*
     * SMTP Service Extension for Authentication
     * https://datatracker.ietf.org/doc/html/rfc4954#section-6
     * 
     * The following error codes may be used to indicate various success or
     * failure conditions. Servers that return enhanced status codes
     * SHOULD use the enhanced codes suggested here.
     */

    private byte authLogin() throws IOException {
        final String usernameLabel = "Username:"; // base64: VXNlcm5hbWU6

        logger.info("S: Awaiting for " + usernameLabel);

        os.write(asciiraw("334 "));
        os.write(Base64.getEncoder().encode(asciiraw(usernameLabel)));
        os.write(ENDLINE);
        os.flush();

        String username = getContent();

        return validateAuthLoginCredential(username);
    }

    private byte authLogin(String statement, byte[] raw) throws IOException {
        if (username.isBlank()) {
            return authLogin();
        }

        if (username.isBlank()) {
            logger.infof("S: %s", SmtpError.INVALID_CREDENTIALS);

            writeLine(os, SmtpError.INVALID_CREDENTIALS);
            os.flush();

            return 0;
        }

        return validateAuthLoginCredential(username);
    }

    private byte validateAuthLoginCredential(String username) throws IOException {
        this.username = new String(Base64.getDecoder().decode(username), StandardCharsets.US_ASCII);
        logger.info("C: Username: " + this.username);

        final String passwordLabel = "Password:"; // base64: UGFzc3dvcmQ6
        logger.info("S: Awaiting for " + passwordLabel);

        os.write(asciiraw("334 "));
        os.write(Base64.getEncoder().encode(asciiraw(passwordLabel)));
        os.write(ENDLINE);
        os.flush();

        final String password = getContent();

        final StringBuilder response = new StringBuilder();

        if (password.trim().isEmpty()) {
            response.append(SmtpError.INVALID_CREDENTIALS);
        } else {
            this.password = new String(Base64.getDecoder().decode(password), StandardCharsets.US_ASCII);
            logger.info("C: Password: " + this.password);

            if (LOCAL_USERNAME.equals(this.username) && LOCAL_PASSWORD.equals(this.password)) {
                this.authenticated = true;
                response.append("235 2.7.0 Authentication Succeeded");
            } else {
                response.append(SmtpError.INVALID_CREDENTIALS);
            }
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

    private byte authPlain(String statement, byte[] raw) throws IOException {
        final String credential = statement.substring(11);

        if (credential.isEmpty()) {
            authPlainTransition();
            return 0;
        }

        return authPlainValidation(credential);
    }

    private byte authPlainTransition() throws IOException {
        final String response = "334 Go ahead";

        writeLine(os, response);
        os.flush();

        final String credential = getContent();
        return authPlainValidation(credential);
    }

    private byte authPlainValidation(String credential) throws IOException {
        final StringBuilder response = new StringBuilder();

        if (credential.trim().isEmpty()) {
            response.append(SmtpError.INVALID_CREDENTIALS);
        } else {
            final byte[] source = asciiraw('\0' + LOCAL_USERNAME + "\0" + LOCAL_PASSWORD);
            final String validCredential = Base64.getEncoder().encodeToString(source);
            if (validCredential.equals(credential)) {
                this.authenticated = true;
                response.append("235 2.7.0 Authentication Succeeded");
            } else {
                response.append(SmtpError.INVALID_CREDENTIALS);
            }
        }

        logger.infof("S: %s", response);

        writeLine(os, response);
        os.flush();

        return 0;
    }

    private byte verifyBadSintax() throws IOException {
        logger.infof("S: %s", SmtpError.MAILBOX_MISSING);

        writeLine(os, SmtpError.MAILBOX_MISSING);
        os.flush();

        return 0;
    }

    static final Mailbox JOHN_EXAMPLE = new Mailbox("John Doe", "john.doe", "example.com");
    static final Mailbox JANE_EXAMPLE = new Mailbox("Jane Doe", "jane.doe", "example.com");
    static final Mailbox MAILING_LIST_EXAMPLE = new Mailbox("mailing@example.com");

    private byte verify(String statement, byte[] raw) throws IOException {
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

        for(final String line: responses)
        {
            logger.infof("S: %s", line);
        }

        for(final String line: responses)
        {
            writeLine(os, line);
        }

        os.flush();

        return 0;
    }

    private byte expand(String statement, byte[] raw) throws IOException {
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

        for(final String line: responses)
        {
            logger.infof("S: %s", line);
        }

        for(final String line: responses)
        {
            writeLine(os, line);
        }
       
        os.flush();

        return 0;
    }

    private Boolean fromHost = null;
    private Mailbox sender = null;

    private byte mailFrom(final String statement, final byte[] raw) throws IOException {
        if(this.introductionHost == null) {
            return introductionMissing();
        }

        if(this.sender != null)
        {
            return senderAlreadySpecified();
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

        this.fromHost = Boolean.FALSE;
        this.whiteList
            .stream()
            .filter(host -> sender.getDomain().equals(host.toLowerCase()))
            .findFirst()
            .ifPresent(q -> fromHost = Boolean.TRUE);

        final StringBuilder response = new StringBuilder();

        if (Boolean.TRUE.equals(fromHost) && !authenticated) {
            response.append(SmtpError.AUTHENTICATION_REQUIRED);
        } else {
            this.recipients.clear();
            this.toHost = null;
            this.sender = sender;
            response.append(String.format("250 2.1.0 <%s>: Originator OK", this.sender.getEmail()));
        }

        logger.infof("S: %s", response);

        writeLine(os, response);
        os.flush();

        return 0;
    }

    private Boolean toHost = null;
    private List<Mailbox> recipients = new LinkedList<>();

    private byte rcptTo(final String statement, final byte[] raw) throws IOException {
        if(this.introductionHost == null)
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

        this.toHost = Boolean.FALSE;
        this.whiteList
                .stream()
                .filter(host -> "relay".equals(host) || recipient.getDomain().equals(host))
                .findFirst()
                .ifPresent(host -> toHost = Boolean.TRUE);

        final List<String> responses = new ArrayList<>();

        if (this.sender == null) {
            responses.add(SmtpError.SENDER_MISSING.toString());
        } else if (Boolean.FALSE.equals(fromHost) && Boolean.FALSE.equals(toHost)) {
            toHost = null;
            // responses.add("551-5.7.1 You've been a naughty guy, right?");
            responses.add("551-5.7.1 Forwarding to remote hosts is not acceptable");
            responses.add("551 5.7.1 Select another host to act as your forwarder");
        } else {
            this.recipients.add(recipient);
            responses.add(String.format("250 2.1.0 <%s>: Recipient OK", recipient.getEmail()));
        }

        for(final String line: responses)
        {
            logger.infof("S: %s", line);
        }

        for(final String line: responses)
        {
            writeLine(os, line);
        }

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
        if(this.introductionHost == null) {
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
            response.append("354 I am ready, send 8BITMIME message, ending in <CRLF>.<CRLF>");
        }

        logger.infof("S: %s", response);

        writeLine(os, response);
        os.flush();

        if (dataInProgress) {
            consumeData();
        }

        return 0;
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

    private byte securityPolicy() throws IOException {
        logger.infof("S: %s", SmtpError.SECURITY_POLICY);

        writeLine(os, SmtpError.SECURITY_POLICY);
        os.flush();

        return 0;
    }

    private byte rset() throws IOException {
        this.authenticated = false;
        this.fromHost = null;
        this.toHost = null;
        this.recipients.clear();

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

    private byte senderAlreadySpecified() throws IOException {
        logger.infof("S: %s", SmtpError.SENDER_ALREADY_SPECIFIED);

        writeLine(os, SmtpError.SENDER_ALREADY_SPECIFIED);
        os.flush();

        return 0;
    }

    final long MAX_MESSAGE_SIZE = 2 * 1024 * 1024;
    private void consumeData() throws IOException {
        final ByteArrayOutputStream data = new ByteArrayOutputStream();

        final int[] control = { -1, -1, -1, -1, -1 };
        int reader = -1;
        while ((reader = is.read()) != -1) {
            control[0] = control[1];
            control[1] = control[2];
            control[2] = control[3];
            control[3] = control[4];
            control[4] = reader;

            if(     control[0] == '\r'
                &&  control[1] == '\n'
                &&  control[2] == '.'
                &&  control[3] == '\r'
                &&  control[4] == '\n'
            )
            {
                dataReceived(data);
                break;
            }

            if( control[4] == '\n' )
            {
                if( control[3] == '\n' )
                {
                    data.write(control[3]);
                    control[3] = -1;
                }

                continue;
            }

            if( control[4] == '\r' )
            {
                if(control[3] == '\r' || control[3] == '\n')
                {
                    data.write(control[3]);
                    control[3] = -1;
                }

                continue;
            }

            if(     control[4] == '.'
                &&  control[3] == '\n'
                &&  control[2] == '\r' )
            {
                continue;
            }

            if(     control[3] == '.'
                &&  control[2] == '\n'
                &&  control[1] == '\r' )
            {
                data.write(control[1]);
                data.write(control[2]);
                control[1] = -1;
                control[2] = -1;
            }

            if( control[2] == '\r' || control[2] == '\n' )
            {
                data.write(control[2]);
                control[2] = -1;
            }

            if( control[3] == '\r' || control[3] == '\n' )
            {
                data.write(control[3]);
                control[3] = -1;
            }

            data.write(reader);
        }

    }

    private byte dataReceived(final ByteArrayOutputStream rawData) throws IOException {
        // Queuing only if this server is a relay, otherwise (final destination),
        // persist data
        final String response = "250 2.6.0 Message accepted";
        logger.infof("S: %s", response);

        writeLine(os, response);
        os.flush();

        final String hash = UUID.randomUUID().toString().replaceAll("\\-", "");

        final File file = new File(this.contentFolder, "mail-" + hash + ".out");

        try (OutputStream outData = new FileOutputStream(file)) {
            final byte[] receivedFrom = String
                    .format("Received: from %s (%s)%s", this.clientHost, this.clientAddress, "\r\n")
                    .getBytes(StandardCharsets.US_ASCII);

            outData.write(receivedFrom);

            final byte[] deliveryDate = String
                    .format("X-Delivery-Date: %s%s", this.timestamp, "\r\n")
                    .getBytes(StandardCharsets.US_ASCII);

            outData.write(deliveryDate);

            rawData.writeTo(outData);

            outData.flush();

        } catch (IOException e) { /***/ }

        logger.infof("--- Data hash: %s ---", hash);

        return 0;
    }

    private void writeLine(final OutputStream out, final Object content) throws IOException
    {
        out.write(asciiraw(content.toString()));
        out.write(ENDLINE);
    }

    private byte[] asciiraw(final CharSequence content)
    {
        return content.toString().getBytes(StandardCharsets.US_ASCII);
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

    static final byte[] ENDLINE = "\r\n".getBytes(StandardCharsets.US_ASCII);
}
