package io.github.smtp.server;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.Base64;

import javax.net.ssl.KeyManager;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;

import org.jboss.logging.Logger;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import io.github.smtp.configs.Configs;
import io.github.smtp.configs.SubmissionConfigs;
import io.github.smtp.protocol.SmtpError;
import io.quarkus.test.junit.QuarkusTest;
import jakarta.inject.Inject;

@QuarkusTest
public class TlsTest extends BaseTest {

    static final int read_timeout = 300;
    static final int connect_timeout = 500;
    static final int start_timeout = 1000;

    @Inject
    Configs configs;

    @Inject
    SubmissionConfigs submissionConfigs;

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
        final Integer port = submissionConfigs.port();

        try
        {
            Thread.sleep(start_timeout);
        } catch(InterruptedException failure)
        {
            Thread.currentThread().interrupt();
            throw new Exception(failure.getMessage());
        }

        int c = 0;
        final String username = "guest@example.com";
        final byte[] encodedUser = Base64.getEncoder().encode(username.getBytes(ASCII));
        final byte[] rawUser = new byte[encodedUser.length + 2];
        for(byte b: encodedUser)
        {
            rawUser[c++] = b;
        }
        rawUser[c++] = '\r';
        rawUser[c++] = '\n';

        c = 0;
        final String password = "wrong@passw0rd!";
        final byte[] encodedPassw = Base64.getEncoder().encode(password.getBytes(ASCII));
        final byte[] rawPassw = new byte[encodedPassw.length + 2];
        for(byte b: encodedPassw)
        {
            rawPassw[c++] = b;
        }
        rawPassw[c++] = '\r';
        rawPassw[c++] = '\n';

        String result = null;

        final InetSocketAddress socketAddress = new InetSocketAddress(hostname, port);

        try (final Socket socket = new Socket()) {
            socket.setSoTimeout(read_timeout);
            socket.connect(socketAddress, connect_timeout);

            final InputStream in = socket.getInputStream();
            final OutputStream out = socket.getOutputStream();

            response(in);

            request("EHLO alpha.net\r\n", out);
            response(in);

            request("STARTTLS\r\n", out);
            result = response(in);
            Assertions.assertEquals("220 2.0.0 Ready to start TLS", result, "Test#0");

            final var sslSocket = upgrade(socket);

            final InputStream sIn = sslSocket.getInputStream();
            final OutputStream sOut = sslSocket.getOutputStream();

            request("EHLO alpha.net\r\n", sOut);
            response(sIn);

            request("AUTH LOGIN\r\n", sOut);
            result = response(sIn);
            Assertions.assertEquals("334 VXNlcm5hbWU6", result, "Test#1");

            request("*\r\n", sOut);
            result = response(sIn);
            Assertions.assertEquals(SmtpError.AUTHENTICATION_ABORTED.toString(), result, "Test#2");

            request("AUTH LOGIN\r\n", sOut);
            result = response(sIn);
            Assertions.assertEquals("334 VXNlcm5hbWU6", result, "Test#3");

            request(username+"\r\n", sOut);
            result = response(sIn);
            Assertions.assertEquals(SmtpError.CANNOT_DECODE_RESPONSE.toString(), result, "Test#4");

            request("AUTH LOGIN\r\n", sOut);
            result = response(sIn);
            Assertions.assertEquals("334 VXNlcm5hbWU6", result, "Test#5");

            sOut.write(rawUser); sOut.flush();
            result = response(sIn);
            Assertions.assertEquals("334 UGFzc3dvcmQ6", result, "Test#6");

            request("*\r\n", sOut);
            result = response(sIn);
            Assertions.assertEquals(SmtpError.AUTHENTICATION_ABORTED.toString(), result, "Test#7");

            request("AUTH LOGIN\r\n", sOut);
            result = response(sIn);
            Assertions.assertEquals("334 VXNlcm5hbWU6", result, "Test#8");

            sOut.write(rawUser); sOut.flush();
            result = response(sIn);
            Assertions.assertEquals("334 UGFzc3dvcmQ6", result, "Test#9");

            sOut.write(rawPassw); sOut.flush();
            result = response(sIn);
            Assertions.assertEquals(SmtpError.INVALID_CREDENTIALS.toString(), result, "Test#10");

            request("AUTH PLAIN\r\n", sOut);
            result = response(sIn);
            Assertions.assertEquals("334 ", result, "Test#11");

            request("*\r\n", sOut);
            result = response(sIn);
            Assertions.assertEquals(SmtpError.AUTHENTICATION_ABORTED.toString(), result, "Test#12");

            request("AUTH PLAIN\r\n", sOut);
            result = response(sIn);
            Assertions.assertEquals("334 ", result, "Test#13");

            request("wrong\r\n", sOut);
            result = response(sIn);
            Assertions.assertEquals(SmtpError.INVALID_CREDENTIALS.toString(), result, "Test#14");

            request("QUIT\r\n", sOut);
            response(sIn);
        }
    }

    private SSLSocket upgrade(final Socket socket)
        throws NoSuchAlgorithmException, KeyManagementException, IOException
    {
        final SSLContext sslContext = SSLContext.getInstance("TLS");
        sslContext.init(
            new KeyManager[] {},
            new TrustManager[] {
                new X509TrustManager() {
                    public void checkClientTrusted(X509Certificate[] arg0, String arg1)
                        throws CertificateException {
                        // OK
                    }

                    public void checkServerTrusted(X509Certificate[] chain, String authType)
                        throws CertificateException {
                        // OK
                    }

                    public X509Certificate[] getAcceptedIssuers() {
                        return null;
                    }
                }
            },
        null);

        return (SSLSocket) sslContext.getSocketFactory()
            .createSocket(
                socket, 
                socket.getInetAddress().getHostAddress(),
                socket.getPort(),
                true
            );
    }

}
