package io.github.rfc5321.logging;

import java.io.PrintStream;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Locale;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

public class LoggerService extends Logger {

    protected LoggerService(String name, String resourceBundleName) {
        super(name, resourceBundleName);
    }

    private LoggerService INSTANCE = this;

    public static Logger getLogger(final String name) {
        return new LoggerService(name);
    }

    private LoggerService(String name) {
        super(name, null);
        addHandler(new Handler() {

            @Override
            public void publish(LogRecord record) {
                INSTANCE.publish(record);
            }

            @Override
            public void flush() {
                INSTANCE.flush();
            }

            @Override
            public void close() throws SecurityException {
                INSTANCE.close();
            }
            
        });
    }

    private void publish(LogRecord record) {
        final String ts = Instant
            .ofEpochMilli(record.getMillis())
            .atZone(ZoneId.systemDefault())
            .format(DateTimeFormatter.ofPattern("EEE, dd MMM yyyy HH:mm:ss Z", Locale.US));

        PrintStream writer = null;

        String levelName = record.getLevel().getName();
        if(Level.SEVERE.equals(record.getLevel())) {
            writer = System.err;
            levelName = "ERROR";
        } else if(Level.WARNING.equals(record.getLevel())) {
            writer = System.err;
        } else {
            writer = System.out;
        }

        writer.print(String.format("%s [%s] [SMTP] %s%n", ts, levelName, record.getMessage()));
    }

    private void flush() {
        // TODO Auto-generated method stub
    }

    private void close() throws SecurityException {
        // TODO Auto-generated method stub
    }
    
}
