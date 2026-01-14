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
import java.util.Base64;

import org.jboss.logging.Logger;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import io.github.smtp.configs.Configs;
import io.quarkus.test.junit.QuarkusTest;
import jakarta.inject.Inject;

@QuarkusTest
public class DataTest {
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

    String response(final InputStream in) throws Exception {
        return content(in);
    }

    @Test
    public void dataTest() throws Exception
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

        final String username = "admin@example.com";
        final String password = "myp@77";

        final byte[] source = ("\0" + username + "\0" + password).getBytes(StandardCharsets.US_ASCII);
        final String validCredential = Base64.getEncoder().encodeToString(source);

        final StringBuilder text = new StringBuilder("");
        text.append("Lorem ipsum dolor sit amet, consectetur adipiscing elit.\r\n");
        text.append("Phasellus gravida est nec ante lobortis, id pellentesque neque maximus.\r\n");
        text.append(".Qhasellus scelerisque a nunc non ultrices. Duis posuere mi eu ullamcorper posuere.\r\n");
        text.append("..Etiam scelerisque nec sem sit amet laoreet. Cras tincidunt eu nibh ullamcorper condimentum.\r\n");
        text.append("...Curabitur sit amet mollis ex, vulputate fermentum odio.\r\n");
        text.append("\r\n");
        text.append("..\r\n");
        text.append("END.\r\n.\r\n");
        final byte[] raw = text.toString().getBytes(StandardCharsets.US_ASCII);

        final InetSocketAddress socketAddress = new InetSocketAddress(hostname, port);

        try (final Socket socket = new Socket()) {
            socket.setSoTimeout(read_timeout);
            socket.connect(socketAddress, connect_timeout);

            final InputStream in = new BufferedInputStream(socket.getInputStream());
            final OutputStream out = new BufferedOutputStream(socket.getOutputStream());

            response(in);

            request(out, "EHLO alpha.net\r\n");
            response(in);

            request(out, "AUTH PLAIN " + validCredential + "\r\n");
            response(in);

            request(out, "MAIL FROM: <alice@example.com>\r\n");
            response(in);

            request(out, "RCPT TO: <bob@example.net>\r\n");
            response(in);

            request(out, "DATA\r\n");
            response(in);

            out.write(raw);
            out.flush();

            response(in);
        }
    }

}
