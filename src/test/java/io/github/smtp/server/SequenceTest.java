package io.github.smtp.server;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
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
import io.github.smtp.protocol.SmtpError;
import io.quarkus.test.junit.QuarkusTest;
import jakarta.inject.Inject;

@QuarkusTest
public class SequenceTest extends BaseTest {
    static final int start_timeout = 250;
    static final int connect_timeout = 500;
    static final int read_timeout = 250;

    @Inject
    Configs configs;

    @Inject
    SmtpAgent server;

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

    void request(final OutputStream out, final String request) throws Exception {
        out.write(request.getBytes(ASCII));
        out.flush();
    }

    String response(final InputStream in) throws Exception {
        return content(in);
    }

    @Test
    public void introductionMissingTest() throws Exception
    {
        final String hostname = configs.server().hostname().orElse("localhost");
        final Integer port = configs.server().port();

        try
        {
            Thread.sleep(start_timeout);
        } catch(InterruptedException failure)
        {
            Thread.currentThread().interrupt();
            throw new Exception(failure.getMessage());
        }

        final InetSocketAddress socketAddress = new InetSocketAddress(hostname, port);

        try (final Socket socket = new Socket()) {
            socket.setSoTimeout(read_timeout);
            socket.connect(socketAddress, connect_timeout);

            final InputStream in = new BufferedInputStream(socket.getInputStream());
            final OutputStream out = new BufferedOutputStream(socket.getOutputStream());

            response(in);

            request(out, "MAIL FROM: <alice@alpha.net>\r\n");
            final String data = response(in);

            Assertions.assertEquals(SmtpError.INTRODUCTION_MISSING.toString(), data);
        }
    }

    @Test
    public void mailFromMissingTest() throws Exception
    {
        final String hostname = configs.server().hostname().orElse("localhost");
        final Integer port = configs.server().port();

        try
        {
            Thread.sleep(start_timeout);
        } catch(InterruptedException failure)
        {
            Thread.currentThread().interrupt();
            throw new Exception(failure.getMessage());
        }

        final InetSocketAddress socketAddress = new InetSocketAddress(hostname, port);

        try (final Socket socket = new Socket()) {
            socket.setSoTimeout(read_timeout);
            socket.connect(socketAddress, connect_timeout);

            final InputStream in = new BufferedInputStream(socket.getInputStream());
            final OutputStream out = new BufferedOutputStream(socket.getOutputStream());

            response(in);

            request(out, "EHLO alpha.net\r\n");
            response(in);

            request(out, "RCPT TO: <bob@beta.net>\r\n");
            final String data = response(in);

            Assertions.assertEquals(SmtpError.SENDER_MISSING.toString(), data);
        }
    }

    @Test
    public void recipientsMissingTest() throws Exception
    {
        final String hostname = configs.server().hostname().orElse("localhost");
        final Integer port = configs.server().port();

        try
        {
            Thread.sleep(start_timeout);
        } catch(InterruptedException failure)
        {
            Thread.currentThread().interrupt();
            throw new Exception(failure.getMessage());
        }

        final InetSocketAddress socketAddress = new InetSocketAddress(hostname, port);

        try (final Socket socket = new Socket()) {
            socket.setSoTimeout(read_timeout);
            socket.connect(socketAddress, connect_timeout);

            final InputStream in = new BufferedInputStream(socket.getInputStream());
            final OutputStream out = new BufferedOutputStream(socket.getOutputStream());

            response(in);

            request(out, "EHLO alpha.net\r\n");
            response(in);

            request(out, "DATA\r\n");
            final String data = response(in);

            Assertions.assertEquals(SmtpError.RECIPIENTS_MISSING.toString(), data);
        }
    }

}
