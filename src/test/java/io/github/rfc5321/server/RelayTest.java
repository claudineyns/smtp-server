package io.github.rfc5321.server;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Random;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;

@TestInstance(Lifecycle.PER_CLASS)
public class RelayTest {
    static final String hostname = "localhost";
    static final int port = 50000 + new Random().nextInt(1000);

    SMTPAgent smtp;

    @BeforeAll
    public void startup() throws Exception {
        System.setProperty("smtp.hostname", hostname);
        System.setProperty("smtp.fqdn.whitelist", "relay");
        System.setProperty("smtp.port", Integer.toString(port));

        System.out.println("[DEBUG] Attempt to connect on port " + port);
        smtp = new SMTPAgent().start();
        Thread.sleep(2000);
    }

    @AfterAll
    public void terminate() throws Exception {
        smtp.stop();
    }

    static final Charset ASCII = StandardCharsets.US_ASCII;

    static final int read_timeout = 1000;
    static final int connect_timeout = 2000;

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
        Thread.sleep(500);
    }

    String response(final InputStream in) throws Exception {
        return content(in);
    }

    @Test
    public void relaySuccess() throws Exception {
        final InetSocketAddress socketAddress = new InetSocketAddress(hostname, port);

        try (final Socket socket = new Socket()) {

            socket.setSoTimeout(read_timeout);
            socket.connect(socketAddress, connect_timeout);

            Thread.sleep(500);

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
