package io.github.rfc5321.server;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.URL;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Properties;
import java.util.Random;

import org.apache.commons.io.IOUtils;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;

import javax.mail.Authenticator;
import javax.mail.BodyPart;
import javax.mail.Message;
import javax.mail.Multipart;
import javax.mail.PasswordAuthentication;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeBodyPart;
import javax.mail.internet.MimeMessage;
import javax.mail.internet.MimeMultipart;

@TestInstance(Lifecycle.PER_CLASS)
public class MailTest {
    static final String hostname = "localhost";
    static final int port = 50000 + new Random().nextInt(1000);

    SMTPAgent smtp;

    @BeforeAll
    public void startup() throws Exception {
        System.setProperty("smtp.hostname", hostname);
        System.setProperty("smtp.fqdn.whitelist", "example.com");
        System.setProperty("smtp.port", Integer.toString(port));

        System.out.println("[DEBUG] Attempt to connect on port " + port);
        smtp = new SMTPAgent().start();
        Thread.sleep(2000);
    }

    @AfterAll
    public void terminate() throws Exception {
        smtp.stop();
    }

    @Test
    public void sendMailSuccess() throws Exception {
        final String username = "postmaster@example.com";
        final String password = "myp@77";

        final Properties prop = new Properties();
        prop.put("mail.smtp.host", "localhost");
        prop.put("mail.smtp.auth", "true");
        prop.put("mail.smtp.port", Integer.toString(port));

        // prop.put("mail.smtp.starttls.enable", "true"); // TLS

        final Session session = Session.getInstance(
                prop,
                new Authenticator() {
                    protected PasswordAuthentication getPasswordAuthentication() {
                        return new PasswordAuthentication(username, password);
                    }
                });

        final Message message = new MimeMessage(session);
        message.setFrom(new InternetAddress(username));
        message.setRecipients(
                Message.RecipientType.TO,
                InternetAddress.parse("johndoe@example.com, janedoe@example.com, jsmith@example.com"));

        message.setSubject("Testing Plain Email");

        final StringBuilder text = new StringBuilder("");
        text.append("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin interdum, turpis ut sollicitudin malesuada, nisi purus molestie ligula, id cursus mauris sapien quis urna. Nulla dui diam, sagittis non feugiat vitae, ultricies a odio. Duis congue enim sit amet metus elementum, mattis egestas erat molestie. Ut vel porta leo. Proin in risus bibendum, bibendum nulla quis, semper dui. Nulla aliquam tristique egestas. Quisque non magna sagittis, vulputate nunc sed, dictum tortor. Fusce ligula quam, efficitur sit amet justo non, laoreet commodo risus. Aliquam nec ante ac nulla vestibulum fermentum non eu leo. Quisque nec odio a tortor imperdiet volutpat id sit amet nulla. Suspendisse consequat hendrerit mattis.");
        text.append("\n\nPhasellus gravida est nec ante lobortis, id pellentesque neque maximus. Curabitur pharetra eu urna at egestas. Morbi finibus lacus in neque rutrum accumsan. Sed vehicula nec magna et finibus. Curabitur mauris sem, dictum vitae ante rutrum, posuere consequat erat. Curabitur lacinia risus imperdiet nulla fringilla faucibus. Nam placerat lacinia urna lacinia sollicitudin. Ut iaculis nibh magna, id faucibus diam vehicula non. Nunc posuere tincidunt leo sed fermentum. Nulla in nulla nulla. Phasellus vel felis odio.");
        text.append("\n\nPhasellus scelerisque a nunc non ultrices. Duis posuere mi eu ullamcorper posuere. Maecenas a sagittis enim. Donec leo ligula, convallis id interdum et, venenatis id orci. Aliquam vestibulum blandit felis et porttitor. Integer commodo porttitor faucibus. Integer felis odio, dictum at lacus a, vulputate dignissim sem. Suspendisse eleifend ornare tellus sit amet tincidunt. Nam sollicitudin volutpat risus, at placerat est pulvinar at. Vestibulum vitae cursus orci, et sodales nisl. Vestibulum blandit lorem vel erat dictum tempus. Vestibulum eu dapibus turpis.");
        text.append("\n\nEtiam scelerisque nec sem sit amet laoreet. Cras tincidunt eu nibh ullamcorper condimentum. Donec tempus ipsum et mi gravida rutrum. Vivamus sed neque ac elit placerat pretium. Pellentesque elementum est quis ex pulvinar interdum. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Nullam pellentesque vel urna nec lobortis. Maecenas vel diam odio. Vestibulum a dui nec lorem malesuada volutpat in non purus. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin interdum fringilla erat vitae pellentesque. Fusce tincidunt consequat venenatis. Proin tempus placerat leo sit amet luctus. Integer eu consectetur tortor. Mauris vestibulum dolor eu nunc sagittis, eu facilisis libero viverra. Praesent sollicitudin tellus ac scelerisque congue.");
        text.append("\n\nCurabitur sit amet mollis ex, vulputate fermentum odio. Aliquam cursus urna purus, vel convallis arcu ullamcorper in. Phasellus commodo finibus lorem sit amet pretium. Vivamus porttitor est vel consectetur pharetra. Interdum et malesuada fames ac ante ipsum primis in faucibus. Aenean sed arcu tempus, ullamcorper quam a, euismod odio. Aenean eget vestibulum massa, id sagittis lectus.");

        BodyPart messageBodyPart = new MimeBodyPart(); 
        messageBodyPart.setText(text.toString());

        final File attachment = new File(System.getProperty("java.io.tmpdir"), "rfc6152.pdf");
        try (final OutputStream content = new FileOutputStream(attachment)) {
            URL url = new URL("https://www.rfc-editor.org/rfc/pdfrfc/rfc6152.txt.pdf");
            IOUtils.copy(url.openStream(), content);
            content.flush();
        }

        MimeBodyPart attachmentPart = new MimeBodyPart();
        attachmentPart.attachFile(attachment);

        Multipart multipart = new MimeMultipart();
        multipart.addBodyPart(messageBodyPart);
        multipart.addBodyPart(attachmentPart);

        message.setContent(multipart);
        Transport.send(message);                
        
        // message.setText(text.toString());
        // Transport.send(message);
    }

    static final Charset ASCII = StandardCharsets.US_ASCII;

    String content(final InputStream in) {
        final StringBuilder data = new StringBuilder("");

        int reader = -1;
        try {
            while((reader = in.read()) != -1) {
                data.append((char)reader);
            }
        } catch(IOException e) {}

        return data.toString();
    }

    void request(final String request, final OutputStream out) throws Exception {
        // System.out.println("[INFO] CLIENT REQUEST\n" + request);
        out.write(request.getBytes(ASCII));
        out.flush();
        Thread.sleep(500);
    }

    String response(final InputStream in) throws Exception {
        String data = content(in);
        Thread.sleep(500);
        return data;
    }

    static final int read_timeout = 1000;
    static final int connect_timeout = 2000;

    @Test
    public void verifyMailboxSuccess() throws Exception {
        final InetSocketAddress socketAddress = new InetSocketAddress(hostname, port);

        try (final Socket socket = new Socket()) {

            socket.setSoTimeout(read_timeout);
            socket.connect(socketAddress, connect_timeout);

            Thread.sleep(500);

            final InputStream in = socket.getInputStream();
            final OutputStream out = socket.getOutputStream();

            response(in);

            request("EHLO example.net\r\n", out);
            response(in);

            request("VRFY john.doe@example.com\r\n", out);
            response(in);

            request("VRFY john.doe\r\n", out);
            response(in);

            request("VRFY jane.doe@example.com\r\n", out);
            response(in);

            request("VRFY doe\r\n", out);
            response(in);

            request("VRFY mailing@example.com\r\n", out);
            response(in);

            request("QUIT\r\n", out);
            response(in);
        }
    }

    @Test
    public void expandMailboxSuccess() throws Exception {
        final InetSocketAddress socketAddress = new InetSocketAddress(hostname, port);

        try (final Socket socket = new Socket()) {

            socket.setSoTimeout(read_timeout);
            socket.connect(socketAddress, connect_timeout);

            Thread.sleep(500);

            final InputStream in = socket.getInputStream();
            final OutputStream out = socket.getOutputStream();

            response(in);

            request("EHLO example.net\r\n", out);
            response(in);

            request("EXPN john.doe@example.com\r\n", out);
            response(in);

            request("EXPN john.doe\r\n", out);
            response(in);

            request("EXPN jane.doe@example.com\r\n", out);
            response(in);

            request("EXPN mailing@example.com\r\n", out);
            response(in);

            request("QUIT\r\n", out);
            response(in);
        }
    }

    @Test
    public void blacklistSuccess() throws Exception {
        final InetSocketAddress socketAddress = new InetSocketAddress(hostname, port);

        try (final Socket socket = new Socket()) {

            socket.setSoTimeout(read_timeout);
            socket.connect(socketAddress, connect_timeout);

            Thread.sleep(500);

            final InputStream in = socket.getInputStream();
            final OutputStream out = socket.getOutputStream();

            response(in);

            request("EHLO example.net\r\n", out);
            response(in);

            request("MAIL FROM:<j.smith@example.net>\r\n", out);
            response(in);

            request("RCPT TO:<postmaster@example.net>\r\n", out);
            response(in);

            request("QUIT\r\n", out);
            response(in);
        }
    }

    @Test
    public void nullSenderSuccess() throws Exception {
        final InetSocketAddress socketAddress = new InetSocketAddress(hostname, port);

        try (final Socket socket = new Socket()) {

            socket.setSoTimeout(read_timeout);
            socket.connect(socketAddress, connect_timeout);

            Thread.sleep(500);

            final InputStream in = socket.getInputStream();
            final OutputStream out = socket.getOutputStream();

            response(in);

            request("EHLO example.net\r\n", out);
            response(in);

            request("MAIL FROM:<>\r\n", out);
            response(in);

            request("RCPT TO:<postmaster@example.com>\r\n", out);
            response(in);

            request("QUIT\r\n", out);
            response(in);
        }
    }

    @Test
    public void invalidClientHostSuccess() throws Exception {
        final InetSocketAddress socketAddress = new InetSocketAddress(hostname, port);

        try (final Socket socket = new Socket()) {

            socket.setSoTimeout(read_timeout);
            socket.connect(socketAddress, connect_timeout);

            Thread.sleep(500);

            final InputStream in = socket.getInputStream();
            final OutputStream out = socket.getOutputStream();

            response(in);

            request("EHLO invalid-domain-example.net\r\n", out);
            response(in);

            request("QUIT\r\n", out);
            response(in);

        }
    }

    @Test
    public void destinationMailboxSuccess() throws Exception {
        final InetSocketAddress socketAddress = new InetSocketAddress(hostname, port);

        try (final Socket socket = new Socket()) {

            socket.setSoTimeout(read_timeout);
            socket.connect(socketAddress, connect_timeout);

            Thread.sleep(500);

            final InputStream in = socket.getInputStream();
            final OutputStream out = socket.getOutputStream();

            response(in);

            request("EHLO example.net\r\n", out);
            response(in);

            request("MAIL FROM:<alice@example.net>\r\n", out);
            response(in);

            request("RCPT TO:<bob@example.com>\r\n", out);
            response(in);

            request("QUIT\r\n", out);
            response(in);

        }
    }

    @Test
    public void sendDataIncompleteMailboxesSuccess() throws Exception {
        final InetSocketAddress socketAddress = new InetSocketAddress(hostname, port);

        try (final Socket socket = new Socket()) {

            socket.setSoTimeout(read_timeout);
            socket.connect(socketAddress, connect_timeout);

            Thread.sleep(500);

            final InputStream in = socket.getInputStream();
            final OutputStream out = socket.getOutputStream();

            response(in);

            request("MAIL FROM: <alice@example.net>\r\n", out);
            response(in);

            request("RCPT TO: <bob@example.com>\r\n", out);
            response(in);

            request("DATA\r\n", out);
            response(in);

            request("EHLO example.net\r\n", out);
            response(in);

            request("DATA\r\n", out);
            response(in);

            request("RCPT TO: <bob@example.com>\r\n", out);
            response(in);

            request("MAIL FROM: <alice@example.net>\r\n", out);
            response(in);

            request("DATA\r\n", out);
            response(in);

            request("QUIT\r\n", out);
            response(in);

        }
    }

}
