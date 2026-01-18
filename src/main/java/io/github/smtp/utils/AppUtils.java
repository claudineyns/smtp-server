package io.github.smtp.utils;

import java.nio.charset.StandardCharsets;
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

    public static boolean matchesStart(final String source, final String other)
    {
        final int oLen = other.length();

        if(oLen > source.length())
        {
            return false;
        }

        return source.regionMatches(true, 0, other, 0, oLen);
    }

    public static boolean matchesEnd(final String source, final String other)
    {
        final int sLen = source.length();
        final int oLen = other.length();

        if(oLen > sLen)
        {
            return false;
        }

        return source.regionMatches(true, sLen - oLen, other, 0, oLen);
    }

    public static byte[] joinEndLine(final byte[]... sources)
    {
        int size = 0;
        for(byte[] q: sources)
        {
            size += q.length;
        }

        final byte[] raw = new byte[size + 2];
        int c = 0;
        for(byte[] q: sources)
        {
            for(byte b: q)
            {
                raw[c++] = b;
            }
        }

        raw[c++] = '\r';
        raw[c++] = '\n';

        return raw;
    }

    public static byte[] asciiraw(final CharSequence content)
    {
        return content.toString().getBytes(StandardCharsets.US_ASCII);
    }

    public static byte[] utf8raw(final CharSequence content)
    {
        return content.toString().getBytes(StandardCharsets.UTF_8);
    }

}
