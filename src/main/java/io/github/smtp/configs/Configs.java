package io.github.smtp.configs;

import java.util.Optional;

import org.eclipse.microprofile.config.inject.ConfigProperty;

import io.github.smtp.application.Mode;
import io.smallrye.config.ConfigMapping;

@ConfigMapping(prefix = "application")
public interface Configs
{
    @ConfigProperty(defaultValue = "STRICT")
    Mode mode();

    Server server();

    Optional<String> contentFolder();

    Optional<String> forbiddenHostWords();

    Optional<String> forbiddenHostMessage();

    interface Server
    {
        Integer port();

        Optional<String> hostname();
        
        Optional<Integer> externalPort();

        Optional<String> fqdnWhitelist();

    }

}
