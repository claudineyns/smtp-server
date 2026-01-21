package io.github.smtp.server;

import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.Socket;
import org.jboss.logging.Logger;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import io.github.smtp.configs.Configs;
import io.quarkus.test.junit.QuarkusTest;

import io.vertx.mutiny.core.Vertx;
import jakarta.inject.Inject;

@QuarkusTest
public class MailTest extends BaseTest {
    @Inject
    Configs configs;

    @Inject
    SmtpAgent server;

    @Inject
    Vertx vertx;

    @Inject
    Logger logger;

    @BeforeEach
    void start() throws Exception
    {
        server.start();
    }

    @AfterEach
    void stop() throws Exception
    {
        server.stop();
    }

    void request(final String request, final OutputStream out) throws Exception {
        out.write(request.getBytes(ASCII));
        out.flush();
    }

    String response(final InputStream in) throws Exception {
        return content(in);
    }

    static final int startup_timeout = 250;
    static final int connect_timeout = 500;
    static final int read_timeout = 250;

    @Test
    public void verifyMailboxSuccessTest() throws Exception {
        final String hostname = configs.server().hostname().orElse("localhost");
        final Integer port = configs.server().port();

        try
        {
            Thread.sleep(startup_timeout);
        } catch(InterruptedException failure)
        {
            Thread.currentThread().interrupt();
            throw new IllegalStateException(failure);
        }

        final InetSocketAddress socketAddress = new InetSocketAddress(hostname, port);

        try (final Socket socket = new Socket()) {
            socket.setSoTimeout(read_timeout);
            socket.connect(socketAddress, connect_timeout);

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
    public void expandMailboxSuccessTest() throws Exception {
        final String hostname = configs.server().hostname().orElse("localhost");
        final Integer port = configs.server().port();

        try
        {
            Thread.sleep(startup_timeout);
        } catch(InterruptedException failure)
        {
            Thread.currentThread().interrupt();
            throw new IllegalStateException(failure);
        }

        final InetSocketAddress socketAddress = new InetSocketAddress(hostname, port);

        try (final Socket socket = new Socket()) {
            socket.setSoTimeout(read_timeout);
            socket.connect(socketAddress, connect_timeout);

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
    public void blacklistSuccessTest() throws Exception {
        final String hostname = configs.server().hostname().orElse("localhost");
        final Integer port = configs.server().port();

        try
        {
            Thread.sleep(startup_timeout);
        } catch(InterruptedException failure)
        {
            Thread.currentThread().interrupt();
            throw new IllegalStateException(failure);
        }

        final InetSocketAddress socketAddress = new InetSocketAddress(hostname, port);

        try (final Socket socket = new Socket()) {
            socket.setSoTimeout(read_timeout);
            socket.connect(socketAddress, connect_timeout);

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
    public void nullSenderSuccessTest() throws Exception {
        final String hostname = configs.server().hostname().orElse("localhost");
        final Integer port = configs.server().port();

        try
        {
            Thread.sleep(startup_timeout);
        } catch(InterruptedException failure)
        {
            Thread.currentThread().interrupt();
            throw new IllegalStateException(failure);
        }

        final InetSocketAddress socketAddress = new InetSocketAddress(hostname, port);

        try (final Socket socket = new Socket()) {
            socket.setSoTimeout(read_timeout);
            socket.connect(socketAddress, connect_timeout);

            final InputStream in = socket.getInputStream();
            final OutputStream out = socket.getOutputStream();

            response(in);

            request("EHLO example.net\r\n", out);
            response(in);

            request("MAIL FROM:<>\r\n", out);
            response(in);

            request("RCPT TO:<admin@example.com>\r\n", out);
            response(in);

            request("QUIT\r\n", out);
            response(in);
        }
    }

    @Test
    public void invalidClientHostSuccessTest() throws Exception {
        final String hostname = configs.server().hostname().orElse("localhost");
        final Integer port = configs.server().port();

        try
        {
            Thread.sleep(startup_timeout);
        } catch(InterruptedException failure)
        {
            Thread.currentThread().interrupt();
            throw new IllegalStateException(failure);
        }

        final InetSocketAddress socketAddress = new InetSocketAddress(hostname, port);

        try (final Socket socket = new Socket()){
            socket.setSoTimeout(read_timeout);
            socket.connect(socketAddress, connect_timeout);

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
    public void destinationMailboxSuccessTest() throws Exception {
        final String hostname = configs.server().hostname().orElse("localhost");
        final Integer port = configs.server().port();

        try
        {
            Thread.sleep(startup_timeout);
        } catch(InterruptedException failure)
        {
            Thread.currentThread().interrupt();
            throw new IllegalStateException(failure);
        }

        final InetSocketAddress socketAddress = new InetSocketAddress(hostname, port);

        try (final Socket socket = new Socket()) {
            socket.setSoTimeout(read_timeout);
            socket.connect(socketAddress, connect_timeout);

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
    public void sendDataIncompleteMailboxesSuccessTest() throws Exception {
        final String hostname = configs.server().hostname().orElse("localhost");
        final Integer port = configs.server().port();

        try
        {
            Thread.sleep(startup_timeout);
        } catch(InterruptedException failure)
        {
            Thread.currentThread().interrupt();
            throw new IllegalStateException(failure);
        }

        final InetSocketAddress socketAddress = new InetSocketAddress(hostname, port);

        try (final Socket socket = new Socket()) {
            socket.setSoTimeout(read_timeout);
            socket.connect(socketAddress, connect_timeout);

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

    @Test
    public void emptyEhloTest() throws Exception {
        final String hostname = configs.server().hostname().orElse("localhost");
        final Integer port = configs.server().port();

        try
        {
            Thread.sleep(startup_timeout);
        } catch(InterruptedException failure)
        {
            Thread.currentThread().interrupt();
            throw new IllegalStateException(failure);
        }

        final InetSocketAddress socketAddress = new InetSocketAddress(hostname, port);

        String data = null;

        try (final Socket socket = new Socket()) {
            socket.setSoTimeout(read_timeout);
            socket.connect(socketAddress, connect_timeout);

            final InputStream in = socket.getInputStream();
            final OutputStream out = socket.getOutputStream();

            response(in);

            request("EHLO\r\n", out);
            data = response(in);
            Assertions.assertEquals(Boolean.TRUE, data.startsWith("250"));

            request("QUIT\r\n", out);
            response(in);
        }
    }

}
