package io.github.smtp.server.test;

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
import io.github.smtp.server.SmtpAgent;
import io.quarkus.test.junit.QuarkusTest;
import jakarta.inject.Inject;

@QuarkusTest
public class RelayTest {
    static final Charset ASCII = StandardCharsets.US_ASCII;

    static final int read_timeout = 300;
    static final int connect_timeout = 500;
    static final int start_timeout = 1000;

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
        out.write(request.getBytes(ASCII));
        out.flush();
    }

    String response(final InputStream in) throws Exception {
        return content(in);
    }

    @Test
    public void relaySuccess() throws Exception
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

            final InputStream in = socket.getInputStream();
            final OutputStream out = socket.getOutputStream();

            response(in);

            request("EHLO alpha.net\r\n", out);
            response(in);

            request("MAIL FROM: <alice@alpha.net>\r\n", out);
            response(in);

            request("RCPT TO: <bob@beta.net>\r\n", out);
            response(in);
        }
    }

}
