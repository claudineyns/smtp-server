package io.github.rfc5321.configs;

import java.util.Optional;

import io.smallrye.config.ConfigMapping;

@ConfigMapping(prefix = "application.smtp.ssl")
public interface SslConfigs {

    String hostname();

    Integer port();

    KeyStore keystore();

    TrustStore truststore();

    Optional<Integer> externalPort();

    interface KeyStore {
        String resource();
        String format();
        String storepass();
    }

    interface TrustStore {
        String resource();
        String format();
        String storepass();
    }

}
