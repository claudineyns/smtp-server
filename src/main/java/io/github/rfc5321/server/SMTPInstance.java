package io.github.rfc5321.server;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Base64;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.logging.Logger;
import java.util.regex.Pattern;

import io.github.rfc5321.logging.LoggerService;

public class SMTPInstance implements Runnable {
    private static final Charset ASCII = StandardCharsets.US_ASCII;

    private final Logger logger = LoggerService.getLogger(getClass().getSimpleName());

    @SuppressWarnings("unused")
    private final UUID sessionId;

    private final Socket socket;
    private InputStream is;
    private OutputStream os;

    private String clientHost;
    private String clientAddress;
    private String localhost;
    private String logFolder;

    private final String timestamp;

    private final List<String> whiteList = new LinkedList<>();

    public SMTPInstance(final Socket socket, final UUID id, final List<String> whiteList) {
        this.socket = socket;
        this.sessionId = id;
        this.whiteList.addAll(whiteList);

        this.timestamp = ZonedDateTime.now(ZoneId.systemDefault())
                .format(DateTimeFormatter.ofPattern("EEE, dd MMM yyyy HH:mm:ss Z", Locale.US));

        this.clientHost = socket.getInetAddress().getHostAddress();
        this.clientAddress = socket.getInetAddress().getHostName();

        log(String.format("Connection from %s [%s]",
                this.clientHost,
                this.clientAddress));

        this.localhost = Optional
                .ofNullable(System.getenv("SMTP_HOSTNAME"))
                .orElse(Optional
                        .ofNullable(System.getProperty("smtp.hostname"))
                        .orElse(System.getenv("HOSTNAME")));

        this.logFolder = Optional
                .ofNullable(System.getenv("SMTP_LOG_FOLDER"))
                .orElse(Optional
                        .ofNullable(System.getProperty("smtp.log.folder"))
                        .orElse(System.getProperty("java.io.tmpdir")));

    }

    private void log(String message) {
        logger.info(String.format("%s", message));
    }

    private void slog(CharSequence message) {
        final String data = message.toString();

        if (Pattern.compile("^\\d{3}\\-").matcher(data).find()) {
            logger.info("S:\n" + data);
        } else {
            logger.info("S: " + data);
        }
    }

    private long last = 0;
    private Object _self = this;
    private boolean closed = false;

    public void run() {
        CompletableFuture.runAsync(() -> checkClosure());

        processRequest();
    }

    private void checkClosure() {
        while (!closed) {
            last = System.currentTimeMillis();
            try {
                synchronized (_self) {
                    _self.wait(40000);
                }
            } catch (InterruptedException e) {
            }
            if ((System.currentTimeMillis() - last) > 30000) {
                close();
                break;
            }
        }
    }

    private void processRequest() {
        try {
            process();
        } catch (IOException e) {
            System.err.println(e.getMessage());
        } finally {
            if (!closed) {
                close();
            }
        }
    }

    private void process() throws IOException {
        is = socket.getInputStream();
        os = socket.getOutputStream();
        startPresentation();
        interact();
    }

    private byte startPresentation() throws IOException {
        final String presentation = "220 " + this.localhost + " ESMTP Ready\r\n";

        os.write(presentation.getBytes(ASCII));
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
                    checkStatement(st);
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

    private byte checkStatement(final ByteArrayOutputStream os) throws IOException {

        last = System.currentTimeMillis();
        synchronized (_self) {
            _self.notifyAll();
        }

        final byte[] raw = os.toByteArray();
        final String statement = new String(raw, ASCII);

        log(String.format("C: %s", statement));

        if ("QUIT".equalsIgnoreCase(statement)) {
            quit(); // will throw exception to close
        }

        if ("HELP".equalsIgnoreCase(statement)) {
            return help();
        }

        if (statement.toUpperCase().startsWith("HELO ")) {
            return helo(statement, raw);
        }

        if (statement.toUpperCase().startsWith("EHLO ")) {
            return ehlo(statement, raw);
        }

        if ("STARTTLS".equalsIgnoreCase(statement)) {
            return startTls();
        }

        if ("VRFY".equalsIgnoreCase(statement)) {
            return verifyBadSintax();
        }

        if (statement.toUpperCase().startsWith("VRFY ")) {
            return verify(statement, raw);
        }

        if ("EXPN".equalsIgnoreCase(statement)) {
            return verifyBadSintax();
        }

        if (statement.toUpperCase().startsWith("EXPN ")) {
            return expand(statement, raw);
        }

        if ("AUTH LOGIN".equalsIgnoreCase(statement)) {
            return authLogin();
        }

        if (statement.toUpperCase().startsWith("AUTH LOGIN ")) {
            return authLogin(statement, raw);
        }

        if ("AUTH PLAIN".equalsIgnoreCase(statement)) {
            return authPlain(statement, raw);
        }

        if (statement.toUpperCase().startsWith("AUTH PLAIN ")) {
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

        return unavailable();
    }

    private byte help() throws IOException {
        final StringBuilder response = new StringBuilder();

        response.append("211 EHLO HELO AUTH MAIL RCPT DATA HELP\r\n");

        os.write(response.toString().getBytes(ASCII));
        os.flush();

        return 0;
    }

    private byte helo(final String statement, final byte[] raw) throws IOException {
        final StringBuilder response = new StringBuilder();

        final String host = statement.substring(5);
        response.append("250 " + this.localhost + " greets " + host + "\r\n");
        slog(response);

        os.write(response.toString().getBytes(ASCII));
        os.flush();

        return 0;
    }
    /*
     * Reply Codes in Numeric Order
     * https://www.rfc-editor.org/rfc/rfc2821#page-45
     */

    private byte ehlo(final String statement, final byte[] raw) throws IOException {
        final StringBuilder response = new StringBuilder();

        final String host = statement.substring(5);

        response.append("250-" + this.localhost + " greets " + host + "\r\n");
        response.append("250-HELP\r\n");
        response.append("250-AUTH PLAIN LOGIN\r\n");
        response.append("250-ENHANCEDSTATUSCODES\r\n");
        response.append("250-8BITMIME\r\n");
        response.append("250-BINARYMIME\r\n");
        response.append("250 CHUNKING\r\n");

        slog(response);

        os.write(response.toString().getBytes(ASCII));
        os.flush();

        return 0;
    }

    // https://docs.oracle.com/cd/E54932_01/doc.705/e54936/cssg_create_ssl_cert.htm#CSVSG180

    private byte startTls() throws IOException {
        final StringBuilder response = new StringBuilder();

        response.append("454 4.7.0 TLS not available due to temporary reason\r\n");
        // response.append("220 Ready to start TLS");
        slog(response);

        os.write(response.toString().getBytes(ASCII));
        os.flush();

        return 0;
    }

    private boolean authenticated = false;
    private String username;
    private String password;
    // private boolean authenticated = true;

    static final String LOCAL_USERNAME = "postmaster@example.com";
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

        StringBuilder response = null;

        response = new StringBuilder();

        final String usernameLabel = "'Username:"; // base64: VXNlcm5hbWU6
        response
                .append("334 ")
                .append(Base64.getEncoder().encodeToString(usernameLabel.getBytes(ASCII)))
                .append("\r\n");

        os.write(response.toString().getBytes(ASCII));
        os.flush();

        String username = getContent();

        return validateAuthLoginCredential(username);
    }

    private byte authLogin(String statement, byte[] raw) throws IOException {
        final String username = statement.substring(11);

        if (username.isEmpty()) {
            return authLogin();
        }

        StringBuilder response = new StringBuilder();

        if (username.trim().isEmpty()) {
            response.append("535 5.7.8 Authentication credentials invalid\r\n");
            slog(response);

            os.write(response.toString().getBytes(ASCII));
            os.flush();
            return 0;
        }

        return validateAuthLoginCredential(username);
    }

    private byte validateAuthLoginCredential(String username) throws IOException {
        this.username = new String(Base64.getDecoder().decode(username), ASCII);

        StringBuilder response = new StringBuilder();

        final String passwordLabel = "Password:"; // base64: UGFzc3dvcmQ6
        response
                .append("334 ")
                .append(Base64.getEncoder().encodeToString(passwordLabel.getBytes(ASCII)))
                .append("\r\n");

        os.write(response.toString().getBytes(ASCII));
        os.flush();

        final String password = getContent();

        response = new StringBuilder();

        if (password.trim().isEmpty()) {
            response.append("535 5.7.8 Authentication credentials invalid\r\n");
        } else {
            this.password = new String(Base64.getDecoder().decode(password), ASCII);

            if (LOCAL_USERNAME.equals(this.username) && LOCAL_PASSWORD.equals(this.password)) {
                this.authenticated = true;
                response.append("235 2.7.0 Authentication Succeeded\r\n");
            } else {
                response.append("500 5.7.0 Authentication credentials invalid\r\n");
            }
        }

        slog(response);

        os.write(response.toString().getBytes(ASCII));
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

        return new String(data.toByteArray(), ASCII);
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
        final StringBuilder response = new StringBuilder();

        response.append("334 Go ahead\r\n");

        os.write(response.toString().getBytes(ASCII));
        os.flush();

        String credential = getContent();

        return authPlainValidation(credential);
    }

    private byte authPlainValidation(String credential) throws IOException {
        final StringBuilder response = new StringBuilder();

        if (credential.trim().isEmpty()) {
            response.append("535 5.7.8  Authentication credentials invalid\r\n");
        } else {
            final String validCredential = Base64.getEncoder()
                    .encodeToString((LOCAL_USERNAME + ":" + LOCAL_PASSWORD).getBytes(ASCII));
            if (validCredential.equals(credential)) {
                this.authenticated = true;
                response.append("235 2.7.0  Authentication Succeeded\r\n");
            } else {
                response.append("535 5.7.8  Authentication credentials invalid\r\n");
            }
        }
        slog(response);

        os.write(response.toString().getBytes(ASCII));
        os.flush();

        return 0;
    }

    private byte verifyBadSintax() throws IOException {
        final StringBuilder response = new StringBuilder();

        response.append("501 5.1.1 Please, provide a mailbox\r\n");
        slog(response);

        os.write(response.toString().getBytes(ASCII));
        os.flush();

        return 0;
    }

    static final Mailbox JOHN_EXAMPLE = new Mailbox("John Doe", "john.doe", "example.com");
    static final Mailbox JANE_EXAMPLE = new Mailbox("Jane Doe", "jane.doe", "example.com");
    static final Mailbox MAILING_LIST_EXAMPLE = new Mailbox("mailing@example.com");

    private byte verify(String statement, byte[] raw) throws IOException {
        final StringBuilder response = new StringBuilder();

        final String mailbox = statement.substring(statement.indexOf(' ') + 1).trim();

        if (MAILING_LIST_EXAMPLE.is(mailbox)) {
            response.append("550 That is a mailing list, not a user");
        } else if ((JOHN_EXAMPLE.getUser()).equals(mailbox) || JOHN_EXAMPLE.is(mailbox)) {
            response.append("250 ").append(JOHN_EXAMPLE.getFullEmail()).append("\r\n");
        } else if ((JANE_EXAMPLE.getUser()).equals(mailbox) || JANE_EXAMPLE.is(mailbox)) {
            response.append("250 ").append(JANE_EXAMPLE.getFullEmail()).append("\r\n");
        } else if ("doe".equals(mailbox)) {
            response.append("553-Ambiguous; Possibilities are\r\n");
            response.append("553-").append(JOHN_EXAMPLE.getFullEmail()).append("\r\n");
            response.append("553 ").append(JANE_EXAMPLE.getFullEmail()).append("\r\n");
        } else {
            response.append("252-2.1.0 Unable to verify user\r\n");
            response.append("252 2.1.0 send some mail, I'll try my best\r\n");
        }

        slog(response);

        os.write(response.toString().getBytes(ASCII));
        os.flush();

        return 0;
    }

    private byte expand(String statement, byte[] raw) throws IOException {
        final StringBuilder response = new StringBuilder();

        final String mailbox = statement.substring(statement.indexOf(' ') + 1).trim();

        if (MAILING_LIST_EXAMPLE.is(mailbox) || MAILING_LIST_EXAMPLE.getUser().equals(mailbox)) {
            response.append("250-").append(JOHN_EXAMPLE.getFullEmail()).append("\r\n");
            response.append("250 ").append(JANE_EXAMPLE.getFullEmail()).append("\r\n");
        } else if ((JOHN_EXAMPLE.getUser()).equals(mailbox) || JOHN_EXAMPLE.is(mailbox)) {
            response.append("550 That is a user name, not a mailing list\r\n");
        } else if ((JANE_EXAMPLE.getUser()).equals(mailbox) || JANE_EXAMPLE.is(mailbox)) {
            response.append("550 That is a user name, not a mailing list\r\n");
        } else {
            response.append("252 2.1.0 Unable to verify mailbox for mailing list\r\n");
        }

        slog(response);

        os.write(response.toString().getBytes(ASCII));
        os.flush();

        return 0;
    }

    private Boolean fromHost = null;
    private Mailbox sender = null;

    private byte mailFrom(final String statement, final byte[] raw) throws IOException {
        final String mailbox = statement.substring(10);

        String name = "";
        String email = mailbox;

        if (mailbox.contains(" ")) {
            name = mailbox.substring(0, mailbox.indexOf('<'));
            email = mailbox.substring(mailbox.indexOf('<'), mailbox.indexOf('>')+1);
        }
        email = email.replaceAll("[<>]", "");

        final String user = email.substring(0, email.indexOf('@'));
        final String domain = email.substring(email.indexOf('@') + 1).toLowerCase();

        this.sender = new Mailbox(name, user, domain);

        this.fromHost = Boolean.valueOf(this.whiteList.isEmpty());
        this.whiteList
                .stream()
                .filter(host -> this.sender.getDomain().equals(host.toLowerCase()))
                .findFirst()
                .ifPresent(q -> fromHost = Boolean.TRUE);

        final StringBuilder response = new StringBuilder();

        if (Boolean.TRUE.equals(fromHost) && !authenticated) {
            response.append("530 5.7.0 Authentication required\r\n");
        } else {
            response.append(String.format("250 2.1.0 <%s>: Originator OK\r\n", this.sender.getEmail()));
        }
        slog(response);

        os.write(response.toString().getBytes(ASCII));
        os.flush();

        return 0;
    }

    private Boolean toHost = null;
    private List<Mailbox> recipients = new LinkedList<>();

    private byte rcptTo(final String statement, final byte[] raw) throws IOException {
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
        this.recipients.add(new Mailbox(name, user, domain));

        this.toHost = Boolean.valueOf(this.whiteList.isEmpty());
        this.whiteList
                .stream()
                .filter(host -> recipient.getDomain().equals(host))
                .findFirst()
                .ifPresent(host -> toHost = Boolean.TRUE);

        final StringBuilder response = new StringBuilder();

        if (this.fromHost == null) {
            response.append("500 5.7.0 Please, identify yourself\r\n");
        } else if (Boolean.TRUE.equals(fromHost) && !authenticated) {
            response.append("530 5.7.0 Authentication required\r\n");
        } else if (Boolean.FALSE.equals(fromHost) && Boolean.FALSE.equals(toHost)) {
            toHost = null;
            response.append("551-5.7.1 You've been a naughty guy, right?\r\n");
            response.append("551-5.7.1 Forwarding to remote hosts is not acceptable\r\n");
            response.append("551 5.7.1 Select another host to act as your forwarder\r\n");
        } else {
            response.append(String.format("250 2.1.0 <%s>: Recipient OK\r\n", recipient.getEmail()));
        }
        slog(response);

        os.write(response.toString().getBytes(ASCII));
        os.flush();

        return 0;
    }

    private byte noop() throws IOException {
        final StringBuilder response = new StringBuilder();

        response.append("250 2.0.0 OK\r\n");
        slog(response);

        os.write(response.toString().getBytes(ASCII));
        os.flush();

        return 0;
    }

    // https://stackoverflow.com/questions/25710599/content-transfer-encoding-7bit-or-8-bit

    private byte data() throws IOException {
        boolean dataInProgress = false;

        final StringBuilder response = new StringBuilder();

        if (this.fromHost == null && this.toHost == null) {
            response.append("554 5.1.0 No valid recipients\r\n");
        } else if (this.fromHost == null) {
            response.append("554 5.1.8 Please, identify yourself\r\n");
        } else if (Boolean.TRUE.equals(this.fromHost) && !this.authenticated) {
            response.append("530 5.7.0 Authentication required\r\n");
        } else if (this.toHost == null) {
            response.append("554 5.1.1 Please, specify a destination mailbox\r\n");
        } else {
            dataInProgress = true;
            // response.append("354 Start mail input; end with <CRLF>.<CRLF>\r\n");
            response.append("354 Send 8BITMIME message, ending in <CRLF>.<CRLF>\r\n");
        }
        slog(response);

        os.write(response.toString().getBytes(ASCII));
        os.flush();

        if (dataInProgress) {
            consumeData();
        }

        return 0;
    }

    private byte unavailable() throws IOException {
        final StringBuilder response = new StringBuilder();

        response.append("550 5.3.0 Unavailable\r\n");
        slog(response);

        os.write(response.toString().getBytes(ASCII));
        os.flush();

        return 0;
    }

    private byte rset() throws IOException {
        this.authenticated = false;
        this.fromHost = null;
        this.toHost = null;

        final StringBuilder response = new StringBuilder();

        response.append("250 2.0.0 OK\r\n");
        slog(response);

        os.write(response.toString().getBytes(ASCII));
        os.flush();

        return 0;
    }

    private byte quit() throws IOException {
        StringBuilder response = new StringBuilder();

        response.append("221-2.0.0 Thank you for your cooperation\r\n");
        response.append("221-2.0.0 " + this.localhost + " Service closing transmission channel\r\n");
        response.append("221 2.0.0 Goodbye\r\n");

        slog(response);

        os.write(response.toString().getBytes(ASCII));
        os.flush();

        throw new IOException("Connection closed by client");
    }

    private void close() {
        try {
            if (!this.closed) {
                this.closed = true;
                socket.close();
            }
        } catch (IOException e) {
        }

        log("Connection closed");
    }

    private void consumeData() throws IOException {
        final ByteArrayOutputStream data = new ByteArrayOutputStream();

        int[] control = { -1, -1, -1, -1, -1 };
        int reader = -1;
        while ((reader = is.read()) != -1) {

            control[0] = control[1];
            control[1] = control[2];
            control[2] = control[3];
            control[3] = control[4];
            control[4] = reader;

            if (control[0] == '\r'
                    && control[1] == '\n'
                    && control[2] == '.'
                    && control[3] == '\r'
                    && control[4] == '\n') {
                data.flush();
                dataReceived(data);
                break;
            }

            data.write(reader);

        }

    }

    private byte dataReceived(final ByteArrayOutputStream rawData) throws IOException {
        final StringBuilder response = new StringBuilder();

        // Queuing only if this server is a relay, otherwise (final destination),
        // persist data
        response.append("250 2.6.0 Message accepted\r\n");

        os.write(response.toString().getBytes(ASCII));
        os.flush();

        final String hash = UUID.randomUUID().toString().replaceAll("\\-", "");

        final File file = new File(this.logFolder, "mail-" + hash + ".out");

        try (OutputStream outData = new FileOutputStream(file)) {
            final byte[] receivedFrom = String.format(
                    "Received: from %s (%s)\n",
                    this.clientHost,
                    this.clientAddress).getBytes(ASCII);

            outData.write(receivedFrom);

            final byte[] deliveryDate = String
                    .format("X-Delivery-Date: %s\n", this.timestamp)
                    .getBytes(ASCII);

            outData.write(deliveryDate);

            rawData.writeTo(outData);

            outData.flush();

        } catch (IOException e) { /***/ }

        log("--- Data hash: " + hash + " ---");

        return 0;
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
            return user + "@" + domain;
        }

        public String getFullEmail() {
            return this.fullname + " <" + this.getEmail() + ">";
        }

        public boolean is(final String email) {
            return getEmail().equals(email);
        }
    }

}
