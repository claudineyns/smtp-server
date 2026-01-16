package io.github.smtp.configs;

import java.util.Optional;

import io.smallrye.config.ConfigMapping;

@ConfigMapping(prefix = "application")
public interface Configs
{
    Server server();

    Optional<String> contentFolder();

    interface Server
    {
        Integer port();

        Optional<String> hostname();
        
        Optional<Integer> externalPort();

        Optional<String> fqdnWhitelist();

    }

}
