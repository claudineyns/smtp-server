package io.github.rfc5321.configs;

import java.util.Optional;

public final class UtilConfigs
{
    public static final Optional<String> OPTIONAL_HOSTNAME = optionalEnv("HOSTNAME");

    public static Optional<String> optionalEnv(final String envname)
    {
        return Optional.ofNullable(System.getenv(envname));
    }
    
}
