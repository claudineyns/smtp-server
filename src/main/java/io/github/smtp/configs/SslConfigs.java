package io.github.smtp.configs;

import java.util.Optional;

import io.smallrye.config.ConfigMapping;

@ConfigMapping(prefix = "application.server.ssl")
public interface SslConfigs
{
    Integer port();

    Optional<Integer> externalPort();

    Keystore keystore();

    interface Keystore
    {
        String resource();
        String storepass();
    }

}
