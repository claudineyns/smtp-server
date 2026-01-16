package io.github.smtp.configs;

import java.util.Optional;

import io.smallrye.config.ConfigMapping;

@ConfigMapping(prefix = "application.server.submission")
public interface SubmissionConfigs
{
    Integer port();

    Optional<Integer> externalPort();
}
