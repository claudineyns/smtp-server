package io.github.rfc5321.utils;

import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

public class AppUtils
{
    
    public static Optional<List<String>> listOf(final Optional<String> source)
    {
        return source.map(q -> Stream
            .of(q.split("\\,"))
            .map(v -> v.replaceAll("[\\r\\n\\s\\t]", ""))
            .filter(v -> ! v.isBlank())
            .toList()
        );
    }
}
