package io.github.rfc5321.server;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Properties;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;

import javax.mail.Authenticator;
import javax.mail.Message;
import javax.mail.PasswordAuthentication;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;

@TestInstance(Lifecycle.PER_CLASS)
public class MailTest {
    static final String hostname = "localhost";
    static final int port = 587;

    SMTPAgent smtp;

    @BeforeAll
    public void startup() throws Exception {
        System.setProperty("smtp.hostname", hostname);
        System.setProperty("smtp.port", Integer.toString(port));

        smtp = new SMTPAgent().start();
        Thread.sleep(5000);
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

        prop.put("mail.smtp.port", "587");
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
                InternetAddress.parse("johndoe@example.com"));

        message.setSubject("Testing Email");
        message.setText("Dear Mail Crawler,\n\n Please do not spam my email!");

        Transport.send(message);

        System.out.println("Done");

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
        System.out.println("[INFO] CLIENT REQUEST\n" + request);
        out.write(request.getBytes(ASCII));
        out.flush();
        Thread.sleep(500);
    }

    void response(final InputStream in) throws Exception {
        String data = content(in);
        System.out.println("\n[INFO] CLIENT RESPONSE\n" + data);
        Thread.sleep(500);
    }

    @Test
    public void verifyMailboxSuccess() throws Exception {
        final InetSocketAddress socketAddress = new InetSocketAddress(hostname, port);

        try (final Socket socket = new Socket()) {

            final int read_timeout = 2000;
            socket.setSoTimeout(read_timeout);

            final int connect_timeout = 5000;
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

            final int read_timeout = 2000;
            socket.setSoTimeout(read_timeout);

            final int connect_timeout = 5000;
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

}
