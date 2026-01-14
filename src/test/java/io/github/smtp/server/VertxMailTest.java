package io.github.smtp.server;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
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
public class VertxMailTest {
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
        final Integer port = configs.server().port();

        final String username = "admin@example.com";
        final String password = "myp@77";

        final StringBuilder text = new StringBuilder("");
        text.append("Lorem ipsum dolor sit amet, consectetur adipiscing elit.\r\n");
        text.append("Phasellus gravida est nec ante lobortis, id pellentesque neque maximus.\r\n");
        text.append("Phasellus scelerisque a nunc non ultrices. Duis posuere mi eu ullamcorper posuere.\r\n");
        text.append("Etiam scelerisque nec sem sit amet laoreet. Cras tincidunt eu nibh ullamcorper condimentum.\r\n");
        text.append("Curabitur sit amet mollis ex, vulputate fermentum odio.");

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
                .setStarttls(StartTLSOptions.REQUIRED)
                //.setStarttls(StartTLSOptions.DISABLED)
                .setTrustAll(true)
                .setUsername(username)
                .setPassword(password);

        final MailClient client = MailClient.create(vertx, config);

        final MailAttachment attachment = MailAttachment.create()
                .setData(Buffer.buffer(raw))
                .setName("rfc6152.pdf")
                .setContentType("application/pdf")
                .setDisposition("attachment");

        final MailMessage message = new MailMessage()
                .setFrom(username)
                .setTo(List.of("johndoe@example.com", "janedoe@example.com", "jsmith@example.com"))
                .setSubject("Testing Plain Email")
                .setHtml(text.toString())
                .setAttachment(List.of(attachment));
        
        try
        {
            client.sendMail(message).await().indefinitely();
            logger.info("E-mail enviado com sucesso!");
        } catch(CompletionException failure)
        {
            logger.warn(failure.getMessage());
        }
    }

}
