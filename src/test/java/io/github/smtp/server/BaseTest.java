package io.github.smtp.server;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

public abstract class BaseTest {
    static final Charset ASCII = StandardCharsets.US_ASCII;

    String content(final InputStream in) throws IOException {
        final ByteArrayOutputStream line = new ByteArrayOutputStream();
        final ByteArrayOutputStream data = new ByteArrayOutputStream();

        int byte0 = 0;
        int byte1 = 0;
        int reader = -1;
        try {
            while((reader = in.read()) != -1) {
                byte0 = byte1;
                byte1 = reader;

                if(byte1 == '\r') continue;

                if(byte0 == '\r' && byte1 == '\n')
                {                    
                    final boolean end = line.toByteArray()[3] == ' ';

                    if(!end)
                    {
                        line.write('\r');
                        line.write('\n');
                    }

                    data.write(line.toByteArray());
                    line.reset();

                    if(!end)
                        continue;
                    else
                        break;
                }

                line.write(reader);
            }
        // } catch(SocketTimeoutException failure) {
        //     // esperado
        } finally { /***/ }

        final byte[] raw = data.toByteArray();

        return new String(raw, ASCII);
    }
    
}
