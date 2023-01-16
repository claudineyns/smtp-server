package io.github.rfc3507;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.logging.LogManager;
import java.util.logging.Logger;

public class SMTPInstance implements Runnable {
    private static final Charset ASCII = StandardCharsets.US_ASCII;

    private final Logger logger = LogManager.getLogManager().getLogger("");
    
    private final UUID sessionId;
    private final Socket socket;

    private InputStream is;
    private OutputStream os;

    private String remoteHost;
    private String remoteAddress;
    private String localhost;
    private String logFolder;

    private final String timestamp;

    private final List<String> whiteList = new LinkedList<>();

    public SMTPInstance(final Socket socket, final UUID id, final List<String> whiteList) {
        
        this.socket = socket;
        this.sessionId = id;
        this.whiteList.addAll(whiteList);
        
        this.timestamp = LocalDateTime
            .now()
            .format(DateTimeFormatter.ofPattern("EEE, dd MMM yyyy HH:mm:ss Z"));

        this.remoteHost = socket.getInetAddress().getHostAddress();
        this.remoteAddress = socket.getInetAddress().getHostName();

        log(String.format("Connection from %s [%s]",
            this.remoteHost,
            this.remoteAddress));

        this.localhost = Optional
            .ofNullable(System.getenv("SMTP_HOSTNAME"))
            .orElse(Optional
                .ofNullable(System.getProperty("smtp.hostname"))
                .orElse(System.getenv("HOSTNAME"))
        );

        this.logFolder = Optional
            .ofNullable(System.getenv("SMTP_LOG_FOLDER"))
            .orElse(Optional
                .ofNullable(System.getProperty("smtp.log.folder"))
                .orElse(System.getProperty("java.io.tmpdir")));

    }

    private void log(String message) {
        logger.info(String.format("[%s] %s", this.sessionId.toString(), message));
    }

    private long last = 0;
    private Object _self = this;
    private boolean closed = false;

    public void run() {
        CompletableFuture.runAsync(()->checkClosure());

        processRequest();
    }

    private void checkClosure() {
        while(!closed) {
            last = System.currentTimeMillis();
            try {
                synchronized(_self) { _self.wait(40000); }
            } catch(InterruptedException e) { }
            if( (System.currentTimeMillis() - last) > 30000 ) {
                close();
                break;
            }
        }
    }

    private void processRequest() {
        try {
            process();
        } catch(IOException e) {
            System.err.println(e.getMessage());
        } finally {
            if(!closed) {
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
        System.out.println("----- presentation -----");

        String presentation = "220 "+this.localhost+" ESMTP Ready\r\n";
        System.out.print(presentation);

        os.write(presentation.getBytes(ASCII));
        os.flush();

        return 0;
    }

    private byte interact() throws IOException {
        final ByteArrayOutputStream st = new ByteArrayOutputStream();

        int reader = -1;
        while((reader = is.read()) != -1) {
            if( reader == '\r' ) { continue; }
            if( reader == '\n' ) {
                try {
                    checkStatement(st);
                } catch(IOException e) {
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
        synchronized(_self) { _self.notifyAll(); }

        final byte[] raw = os.toByteArray();
        final String statement = new String(raw, ASCII);

        log(String.format("Request: %s", statement));

        if( "QUIT".equalsIgnoreCase(statement) ) {
            quit(); // will throw exception to close
        }

        System.out.println("--- response ---");

        if( "HELP".equalsIgnoreCase(statement) ) {
            return help();
        }

        if( statement.toUpperCase().startsWith("HELO ") ) {
            return ehlo(statement, raw);
        }

        if( statement.toUpperCase().startsWith("EHLO ") ) {
            return ehlo(statement, raw);
        }

        if( "STARTTLS".equalsIgnoreCase(statement) ) {
            return startTls();
        }

        if( statement.toUpperCase().startsWith("VRFY ") ) {
            return verify(statement, raw);
        }

        if( "AUTH LOGIN".equalsIgnoreCase(statement) ) {
            return authLogin();
        }

        if( statement.toUpperCase().startsWith("AUTH LOGIN ") ) {
            return authLogin(statement, raw);
        }

        if( "AUTH PLAIN".equalsIgnoreCase(statement) ) {
            return authPlain(statement, raw);
        }

        if( statement.toUpperCase().startsWith("AUTH PLAIN ") ) {
            return authPlain(statement, raw);
        }

        if( statement.toUpperCase().startsWith("MAIL FROM:") ) {
            return mailFrom(statement, raw);
        }

        if( statement.toUpperCase().startsWith("RCPT TO:") ) {
            return rcptTo(statement, raw);
        }

        if( "NOOP".equalsIgnoreCase(statement) ) {
            return noop();
        }

        if( "DATA".equalsIgnoreCase(statement) ) {
            return data();
        }

        if( "RSET".equalsIgnoreCase(statement) ) {
            return rset();
        }

        return unavailable();
    }

    private byte help() throws IOException {
        final StringBuilder response = new StringBuilder();
        response.append("250 EHLO HELO AUTH MAIL RCPT DATA HELP\r\n");
        System.out.print(response);

        os.write(response.toString().getBytes(ASCII));
        os.flush();

        return 0;
    }

    private byte ehlo(final String statement, final byte[]raw ) throws IOException {
        final String host = statement.substring(5);

        final StringBuilder response = new StringBuilder();
        response.append("250-"+this.localhost+" greets "+host+"\r\n");
        response.append("250-AUTH PLAIN LOGIN\r\n");
        response.append("250-ENHANCEDSTATUSCODES\r\n");
        response.append("250 HELP\r\n");
        System.out.print(response);

        os.write(response.toString().getBytes(ASCII));
        os.flush();

        return 0;
    }

    private byte startTls() throws IOException {
        final StringBuilder response = new StringBuilder();

        response.append("454 TLS not available due to temporary reason\r\n");
        System.out.print(response);

        os.write(response.toString().getBytes(ASCII));
        os.flush();

        return 0;
    }

    private boolean authenticated = false;

    private byte authLogin() throws IOException {

        StringBuilder response = null;
        
        response = new StringBuilder();
        
        //username
        response.append("334 VXNlcm5hbWU6\r\n");
        
        System.out.print(response);
        
        os.write(response.toString().getBytes(ASCII));
        os.flush();

        String username = getContent();
        
        return validateAuthLoginCredential(username);
    }

    private byte authLogin(String statement, byte[]raw) throws IOException {
        final String username = statement.substring(11);

        if( username.isEmpty() ) {
            return authLogin();
        }

        StringBuilder response = new StringBuilder();

        if( username.trim().isEmpty() ) {
            response.append("535 5.7.8 Username cannot be empty\r\n");
            System.out.print(response);   
            os.write(response.toString().getBytes(ASCII));
            os.flush();
            return 0;
        }

        return validateAuthLoginCredential(username);
    }

    private byte validateAuthLoginCredential(String username) throws IOException {
        log(String.format("Username: %s", username));

        StringBuilder response = new StringBuilder();

        // password
        response.append("334 UGFzc3dvcmQ6\r\n");
        
        System.out.print(response);
        
        os.write(response.toString().getBytes(ASCII));
        os.flush();

        String password = getContent();

        response = new StringBuilder();
        
        if( password.trim().isEmpty() ) {
            response.append("535 5.7.8 Password cannot be empty\r\n");
        } else {
            log(String.format("Password: %s", password));
            //authenticated = true;
            //response.append("235 2.7.0 Authentication successful\r\n");
            response.append("535 5.7.8 Authentication credentials invalid\r\n");
        }

        System.out.print(response);
        
        os.write(response.toString().getBytes(ASCII));
        os.flush();

        return 0;
    }

    private String getContent() throws IOException {

        ByteArrayOutputStream data = new ByteArrayOutputStream();

        int[] control = {-1, -1};
        int reader = -1;
        while((reader = is.read()) != -1) {

            control[0] = control[1];
            control[1] = reader;

            if( reader == '\r' ) {
                continue;
            }

            if( control[0] == '\r' && control[1] == '\n'  
            ) {
                break;
            } 

            data.write(reader);

        }

        return new String(data.toByteArray(), ASCII);

    }

    private byte authPlain(String statement, byte[]raw) throws IOException {
        final String credential = statement.substring(11);

        if( credential.isEmpty() ) {
            authPlainTransition();
            return 0;
        }

        return authPlainValidation(credential);
    }
    
    private void authPlainTransition() throws IOException {

        StringBuilder response = new StringBuilder();
        //response.append("334 \r\n");
        response.append("334 Go ahead\r\n");
        System.out.print(response);

        os.write(response.toString().getBytes(ASCII));
        os.flush();

        String credential = getContent();

        authPlainValidation(credential);

    }

    private byte authPlainValidation(String credential) throws IOException {
        final StringBuilder response = new StringBuilder();

        if( credential.trim().isEmpty() ) {
            response.append("535 5.7.8 Authentication credentials invalid\r\n");
        } else {
        //    authenticated = true;
        //    response.append("235 2.7.0 Authentication successful\r\n");
            response.append("535 5.7.8 Authentication credentials invalid\r\n");
        }

        System.out.print(response);

        System.out.println("--- credential ---");
        System.out.println(credential);

        os.write(response.toString().getBytes(ASCII));
        os.flush();

        return 0;
    }

    private byte verify(String statement, byte[]raw) throws IOException {
        final StringBuilder response = new StringBuilder();

        response.append("252 send some mail, I'll try my best\r\n");    

        System.out.print(response);

        os.write(response.toString().getBytes(ASCII));
        os.flush();

        return 0;
    }

    private Boolean fromHost = null;

    private byte mailFrom(final String statement, final byte[]raw) throws IOException {
        this.fromHost = Boolean.FALSE;

        final String from = statement.substring(10);

        for(String host: whiteList) {
            if( from.toLowerCase().contains('@'+host.toLowerCase()) ){
                fromHost = Boolean.TRUE;
                break;
            }
        }

        final StringBuilder response = new StringBuilder();

        if( Boolean.TRUE.equals(fromHost) && !authenticated ) {
            response.append("530 5.7.0 Authentication required\r\n");    
        } else {            
            response.append(String.format("250 2.1.0 Originator %s OK\r\n", from));
        }
        
        System.out.print(response);

        os.write(response.toString().getBytes(ASCII));
        os.flush();

        return 0;
    }

    private Boolean toHost = null;

    private byte rcptTo(final String statement, final byte[]raw) throws IOException {
        final String to = statement.substring(8);

        this.toHost = Boolean.FALSE;

        for(String host: whiteList) {
            if( to.toLowerCase().contains('@'+host.toLowerCase()) ){
                toHost = Boolean.TRUE;
                break;
            }
        }

        final StringBuilder response = new StringBuilder();

        if( this.fromHost == null ) {
            response.append("550 Please, identify yourself\r\n");    
        } else if( Boolean.TRUE.equals(fromHost) && !authenticated ) {
            response.append("530 5.7.0 Authentication required\r\n");    
        } else if( Boolean.FALSE.equals(fromHost) && Boolean.FALSE.equals(toHost) ) {
            toHost = null;
            //response.append("550 Relay not allowed here\r\n");    
            response.append("551-5.7.1 You've been a naughty guy, right?\r\n");
            response.append("551-5.7.1 Forwarding to remote hosts is not acceptable\r\n");
            response.append("551 5.7.1 Select another host to act as your forwarder\r\n");
        } else {
            response.append(String.format("250 2.1.5 Recipient %s OK\r\n", to));
        }

        System.out.print(response);

        os.write(response.toString().getBytes(ASCII));
        os.flush();

        return 0;
    }

    private byte noop() throws IOException {
        StringBuilder response = new StringBuilder();

        response.append("250 2.0.0 OK\r\n");
        System.out.print(response);

        os.write(response.toString().getBytes(ASCII));
        os.flush();

        return 0;
    }
    
    private byte data() throws IOException {
        boolean dataInProgress = false;

        final StringBuilder response = new StringBuilder();

        if( fromHost == null ) {
            response.append("550 Please, identify yourself\r\n");    
        } else if( Boolean.TRUE.equals(fromHost) && !authenticated ) {
            response.append("530 5.7.0 Authentication required\r\n");    
        } else if( toHost == null ) {
            response.append("550 Please, specify a destination mailbox\r\n");    
        } else {
            dataInProgress = true;
            response.append("354 Start mail input; end with <CRLF>.<CRLF>\r\n");
        }
        
        System.out.print(response);

        os.write(response.toString().getBytes(ASCII));
        os.flush();

        if( dataInProgress ) {
            consumeData();
        }

        return 0;
    }

    private byte unavailable() throws IOException {
        final StringBuilder response = new StringBuilder();

        response.append("550 Unavailable\r\n");
        System.out.print(response);

        os.write(response.toString().getBytes(ASCII));
        os.flush();

        return 0;
    }

    private byte rset() throws IOException {
        this.authenticated = false;
        this.fromHost = null;
        this.toHost = null;

        final StringBuilder response = new StringBuilder();

        response.append("250 2.0.0 Reseting\r\n");
        System.out.print(response);

        os.write(response.toString().getBytes(ASCII));
        os.flush();

        return 0;
    }

    private byte quit() throws IOException {
        StringBuilder response = new StringBuilder();

        response.append("221-2.0.0 Thank you for your cooperation\r\n");
        response.append("221-2.0.0 "+this.localhost+" Service closing transmission channel\r\n");
        response.append("221 2.0.0 Goodbye\r\n");

        System.out.print(response);

        os.write(response.toString().getBytes(ASCII));
        os.flush();

        throw new IOException("Connection closed by client");
    }

    private void close() {
        try {
            if(!this.closed) {
                this.closed = true;
                socket.close();
            }
        } catch(IOException e) {}

        log("Connection closed");
    }

    private void consumeData() throws IOException {
        ByteArrayOutputStream data = new ByteArrayOutputStream();

        int[] control = {-1, -1, -1, -1, -1};
        int reader = -1;
        while((reader = is.read()) != -1) {

            control[0]=control[1];
            control[1]=control[2];
            control[2]=control[3];
            control[3]=control[4];
            control[4] = reader;

            if(    control[0] == '\r' 
                && control[1] == '\n'  
                && control[2] == '.'  
                && control[3] == '\r'  
                && control[4] == '\n'  
            ) {
                data.flush();
                byte[] raw = data.toByteArray();
                dataReceived(Arrays.copyOf(raw, raw.length-4));
                break;
            } 

            data.write(reader);

        }

    }

    private byte dataReceived(final byte[] raw) throws IOException {
        final StringBuilder response = new StringBuilder();

        response.append("250 2.6.0 Message accepted\r\n");
        System.out.print(response);

        os.write(response.toString().getBytes(ASCII));
        os.flush();

        final String hash = UUID.randomUUID().toString().replaceAll("\\-","");

        final File file = new File(this.logFolder, "mail-"+hash+".out");

        try (OutputStream outData = new FileOutputStream(file)) {
            outData.write(String.format("Received: from %s (%s)\n", 
                this.remoteHost, this.remoteAddress).getBytes());
            outData.write(String.format("X-Delivery-Date: %s\n", this.timestamp).getBytes());
            outData.write(raw);
            outData.flush();
        } catch(IOException e) { }

        System.out.println("--- data ---");
        System.out.println(hash);

        return 0;
    }

}
