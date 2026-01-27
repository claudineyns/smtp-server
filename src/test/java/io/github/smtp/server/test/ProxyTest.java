package io.github.smtp.server.test;

import io.quarkus.test.junit.QuarkusTest;
import jakarta.inject.Inject;

import java.io.IOException;
import java.net.Inet4Address;
import java.net.InetSocketAddress;
import java.net.Proxy;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;

import javax.net.ssl.KeyManager;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;

import org.jboss.logging.Logger;
import org.junit.jupiter.api.Test;

//@QuarkusTest
public class ProxyTest
{
    @Inject
    Logger logger;

    //@Test
    void test() throws Exception
    {
        final var torInetAddress = Inet4Address.getLoopbackAddress();
        final var torSocketAddress = new InetSocketAddress(torInetAddress, 9050);
        final var torProxy = new Proxy(Proxy.Type.SOCKS, torSocketAddress);

        final var endpointHost = InetSocketAddress.createUnresolved("check.torproject.org", 443);

        logger.info("Creating socket...");        
        final var socket = new Socket(torProxy);
        socket.setSoTimeout(30000);

        logger.info("Connecting...");
        
        socket.connect(endpointHost, 10000);

        if (socket.isConnected())
        {
            logger.info("Connection established! DNS has been resolved by SOCKS server.");
        }

        final StringBuilder response = new StringBuilder();

        logger.info("Upgrading socket...");
        try(final var sslSocket = upgradeSocket(socket, endpointHost))
        {
            final var params = sslSocket.getSSLParameters();
            params.setProtocols(new String[] {"TLSv1.1", "TLSv1.2"});

            sslSocket.setSSLParameters(params);
            sslSocket.setUseClientMode(true);

            sslSocket.addHandshakeCompletedListener(event -> logger.info("Ready to flow data"));

            sslSocket.startHandshake();

            final var in = sslSocket.getInputStream();
            final var out = sslSocket.getOutputStream();

            final StringBuilder request = new StringBuilder();
            request.append("GET /api/ip HTTP/1.1\r\n");
            request.append("Host: " + endpointHost.getHostString() + ":443\r\n");
            request.append("User-Agent: Java/21\r\n");
            request.append("Content-Length: 0\r\n");
            request.append("Connection: close\r\n");
            request.append("\r\n");

            logger.info("Sending...");
            logger.info("\n" + request.toString() + "\n");

            out.write(asciiraw(request));
            out.flush();

            logger.info("Receiving...");
            int reader = -1;
            while((reader = in.read()) != -1)
            {
                response.append((char)reader);
            }

        } catch(IOException failure)
        {
            // OK
        }

        logger.info(">>> response <<<");
        logger.info("\n" + response.toString() + "\n");

        logger.info("All done!");
    }

    private SSLSocket upgradeSocket(final Socket socket, final InetSocketAddress inetHost)
        throws NoSuchAlgorithmException, KeyManagementException, IOException
    {
        final var remoteSocketAddress = ((InetSocketAddress) socket.getRemoteSocketAddress());

        final SSLContext sslContext = SSLContext.getInstance("TLS");
        sslContext.init(
            new KeyManager[] {},
            new TrustManager[] {
                new X509TrustManager() {
                    public void checkClientTrusted(X509Certificate[] chain, String authType)
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

        final String host = inetHost.getHostString();
        final int port = remoteSocketAddress.getPort();

        logger.infof("Upgrading for %s:%d", host, port);

        return (SSLSocket) sslContext.getSocketFactory()
            .createSocket(
                socket, 
                host,
                port,
                true
            );
    }

    static byte[] asciiraw(final Object source)
    {
        return source.toString().getBytes(StandardCharsets.US_ASCII);
    }

}
