package io.github.smtp.server;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.concurrent.CompletionException;

import org.jboss.logging.Logger;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import io.github.smtp.configs.Configs;
import io.quarkus.test.junit.QuarkusTest;

import io.vertx.ext.mail.MailConfig;
import io.vertx.ext.mail.MailMessage;
import io.vertx.ext.mail.MailAttachment;
import io.vertx.ext.mail.LoginOption;
import io.vertx.ext.mail.StartTLSOptions;
import io.vertx.mutiny.core.Vertx;
import io.vertx.mutiny.ext.mail.MailClient;
import jakarta.inject.Inject;
import io.vertx.core.buffer.Buffer;

@QuarkusTest
public class MailTest {
    @Inject
    Configs configs;

    @Inject
    SMTPAgent server;

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

    @Test
    public void sendMailSuccess() throws Exception {
        final String hostname = configs.server().hostname().orElse("localhost");
        final Integer port = configs.server().port().orElse(25);

        final String username = "postmaster@example.com";
        final String password = "myp@77";

        final StringBuilder text = new StringBuilder("");
        text.append("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin interdum, turpis ut sollicitudin malesuada, nisi purus molestie ligula, id cursus mauris sapien quis urna. Nulla dui diam, sagittis non feugiat vitae, ultricies a odio. Duis congue enim sit amet metus elementum, mattis egestas erat molestie. Ut vel porta leo. Proin in risus bibendum, bibendum nulla quis, semper dui. Nulla aliquam tristique egestas. Quisque non magna sagittis, vulputate nunc sed, dictum tortor. Fusce ligula quam, efficitur sit amet justo non, laoreet commodo risus. Aliquam nec ante ac nulla vestibulum fermentum non eu leo. Quisque nec odio a tortor imperdiet volutpat id sit amet nulla. Suspendisse consequat hendrerit mattis.");
        text.append("\n\nPhasellus gravida est nec ante lobortis, id pellentesque neque maximus. Curabitur pharetra eu urna at egestas. Morbi finibus lacus in neque rutrum accumsan. Sed vehicula nec magna et finibus. Curabitur mauris sem, dictum vitae ante rutrum, posuere consequat erat. Curabitur lacinia risus imperdiet nulla fringilla faucibus. Nam placerat lacinia urna lacinia sollicitudin. Ut iaculis nibh magna, id faucibus diam vehicula non. Nunc posuere tincidunt leo sed fermentum. Nulla in nulla nulla. Phasellus vel felis odio.");
        text.append("\n\nPhasellus scelerisque a nunc non ultrices. Duis posuere mi eu ullamcorper posuere. Maecenas a sagittis enim. Donec leo ligula, convallis id interdum et, venenatis id orci. Aliquam vestibulum blandit felis et porttitor. Integer commodo porttitor faucibus. Integer felis odio, dictum at lacus a, vulputate dignissim sem. Suspendisse eleifend ornare tellus sit amet tincidunt. Nam sollicitudin volutpat risus, at placerat est pulvinar at. Vestibulum vitae cursus orci, et sodales nisl. Vestibulum blandit lorem vel erat dictum tempus. Vestibulum eu dapibus turpis.");
        text.append("\n\nEtiam scelerisque nec sem sit amet laoreet. Cras tincidunt eu nibh ullamcorper condimentum. Donec tempus ipsum et mi gravida rutrum. Vivamus sed neque ac elit placerat pretium. Pellentesque elementum est quis ex pulvinar interdum. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Nullam pellentesque vel urna nec lobortis. Maecenas vel diam odio. Vestibulum a dui nec lorem malesuada volutpat in non purus. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin interdum fringilla erat vitae pellentesque. Fusce tincidunt consequat venenatis. Proin tempus placerat leo sit amet luctus. Integer eu consectetur tortor. Mauris vestibulum dolor eu nunc sagittis, eu facilisis libero viverra. Praesent sollicitudin tellus ac scelerisque congue.");
        text.append("\n\nCurabitur sit amet mollis ex, vulputate fermentum odio. Aliquam cursus urna purus, vel convallis arcu ullamcorper in. Phasellus commodo finibus lorem sit amet pretium. Vivamus porttitor est vel consectetur pharetra. Interdum et malesuada fames ac ante ipsum primis in faucibus. Aenean sed arcu tempus, ullamcorper quam a, euismod odio. Aenean eget vestibulum massa, id sagittis lectus.");

        final HttpClient http = HttpClient.newHttpClient();

        // Lista da OpenAI
        final HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create("https://www.rfc-editor.org/rfc/pdfrfc/rfc6152.txt.pdf"))
                .build();

        final byte[] raw = http.send(request, HttpResponse.BodyHandlers.ofByteArray()).body();

        final MailConfig config = new MailConfig()
                .setHostname(hostname)
                .setPort(port)
                .setLogin(LoginOption.REQUIRED)
                //.setStarttls(StartTLSOptions.REQUIRED)
                .setStarttls(StartTLSOptions.DISABLED)
                .setUsername(username)
                .setPassword(password);

        final MailClient client = MailClient.create(vertx, config);

        final MailAttachment attachment = MailAttachment.create()
                .setData(Buffer.buffer(raw))
                .setName("rfc6152.txt.pdf")
                .setContentType("application/pdf") // Ajuste conforme o tipo do arquivo
                .setDisposition("attachment");

        final MailMessage message = new MailMessage()
                .setFrom(username)
                .setTo(List.of("johndoe@example.com", "janedoe@example.com", "jsmith@example.com"))
                .setSubject("Testing Plain Email")
                .setHtml(text.toString())
                .setAttachment(List.of(attachment));

        // 4. Enviar e aguardar (importante para @QuarkusMain não fechar antes)
        try
        {
            client.sendMail(message).await().indefinitely();
            logger.info("E-mail enviado com sucesso!");
        } catch(CompletionException failure)
        {
            logger.warn(failure.getMessage());
        }

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
        out.write(request.getBytes(ASCII));
        out.flush();
        Thread.sleep(500);
    }

    String response(final InputStream in) throws Exception {
        return content(in);
    }

    static final int read_timeout = 250;
    static final int connect_timeout = 1000;
    static final int startup_timeout = 1000;

    @Test
    public void verifyMailboxSuccess() throws Exception {
        final String hostname = configs.server().hostname().orElse("localhost");
        final Integer port = configs.server().port().orElse(25);

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
        final String hostname = configs.server().hostname().orElse("localhost");
        final Integer port = configs.server().port().orElse(25);

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
        final String hostname = configs.server().hostname().orElse("localhost");
        final Integer port = configs.server().port().orElse(25);

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
        final String hostname = configs.server().hostname().orElse("localhost");
        final Integer port = configs.server().port().orElse(25);

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
        final String hostname = configs.server().hostname().orElse("localhost");
        final Integer port = configs.server().port().orElse(25);

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
        final String hostname = configs.server().hostname().orElse("localhost");
        final Integer port = configs.server().port().orElse(25);

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
        final String hostname = configs.server().hostname().orElse("localhost");
        final Integer port = configs.server().port().orElse(25);

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
