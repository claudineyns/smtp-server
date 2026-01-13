package io.github.smtp.configs;

import java.util.Optional;

import io.smallrye.config.ConfigMapping;

@ConfigMapping(prefix = "application")
public interface Configs
{
    Server server();

    interface Server
    {
        Optional<String> hostname();

        Optional<Integer> port();

        Optional<Integer> externalPort();

        Optional<Fqdn> fqdn();

        interface Fqdn
        {
            Optional<String> whitelist();
        }
    }

}
