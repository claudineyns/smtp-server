package io.github.smtp.application;

import org.jboss.logging.Logger;

import io.github.smtp.server.SmtpAgent;
import io.quarkus.runtime.Quarkus;
import io.quarkus.runtime.QuarkusApplication;
import io.quarkus.runtime.annotations.QuarkusMain;
import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
import jakarta.inject.Inject;

@QuarkusMain
public class Application implements QuarkusApplication
{
    @Inject
    Logger logger;

    @Inject
    SmtpAgent server;

    @PostConstruct
    void startup()
    {
        prepare();
        server.start();
    }

    @PreDestroy
    void finish()
    {        
        server.stop();
    }

    private void prepare()
    {
        // Security engineering (for TLSv1.2)
        // Block renegotiation started by client in order to avoid DoS.
        // The server can still start renegotiation if needs (rarely), but not the client.
        System.setProperty("jdk.tls.rejectClientInitiatedRenegotiation", "true");
    }

    public int run(String... args) throws Exception
    {
        logger.info(">>> Quarkus application started <<<");

        Quarkus.waitForExit();

        logger.info(">>> Quarkus application about to finish <<<");

        // Return 0 for "success" or other for error, and application will be finished.
        return 0;
    }

}
