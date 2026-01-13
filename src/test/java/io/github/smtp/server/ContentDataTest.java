package io.github.smtp.server;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

import org.jboss.logging.Logger;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import io.github.smtp.configs.Configs;
import io.quarkus.test.junit.QuarkusTest;
import jakarta.inject.Inject;

@QuarkusTest
public class ContentDataTest {
    static final Charset ASCII = StandardCharsets.US_ASCII;

    static final int read_timeout = 250;
    static final int connect_timeout = 500;
    static final int start_timeout = 500;

    @Inject
    Configs configs;

    @Inject
    SMTPAgent server;

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

    String content(final InputStream in) {
        final ByteArrayOutputStream data = new ByteArrayOutputStream();

        int reader = -1;
        try {
            while((reader = in.read()) != -1) {
                data.write(reader);
            }
        } catch(IOException e) {}

        final byte[] raw = data.toByteArray();

        if(raw.length == 0)
        {
            return new String(raw, ASCII);
        }

        if(raw[raw.length-2] == '\r' && raw[raw.length-1] == '\n')
        {
            return new String(raw, 0, raw.length - 2, ASCII);
        }

        return new String(raw, ASCII);
    }

    void request(final OutputStream out, final String request) throws Exception {
        out.write(request.getBytes(ASCII));
        out.flush();
        Thread.sleep(250);
    }

    void write(final OutputStream out, final String request) throws Exception {
        out.write(request.getBytes(ASCII));
        out.flush();
    }

    String response(final InputStream in) throws Exception {
        return content(in);
    }

    @Test
    public void sendContentTest() throws Exception
    {
        final String hostname = configs.server().hostname().orElse("localhost");
        final Integer port = configs.server().port().orElse(25);

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

            request(out, "MAIL FROM: <alice@example.com>\r\n");
            response(in);

            request(out, "RCPT TO: <bob@example.com>\r\n");
            response(in);

            request(out, "DATA\r\n");
            response(in);
        }
    }

}
