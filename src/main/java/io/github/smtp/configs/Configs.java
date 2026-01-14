package io.github.smtp.configs;

import java.util.Optional;

import io.smallrye.config.ConfigMapping;

@ConfigMapping(prefix = "application")
public interface Configs
{
    Optional<String> contentFolder();

    Server server();

    interface Server
    {
        Optional<String> hostname();

        Integer port();

        Optional<Integer> externalPort();

        Optional<Fqdn> fqdn();

        interface Fqdn
        {
            Optional<String> whitelist();
        }

    }

}
