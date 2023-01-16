package io.github.rfc3507;

import java.io.PrintStream;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;

public class LoggerService extends Handler {

    @Override
    public void publish(LogRecord record) {
        final String ts = Instant
            .ofEpochMilli(record.getMillis())
            .atZone(ZoneId.systemDefault())
            .toLocalDateTime()
            .format(DateTimeFormatter.ofPattern("EEE, dd MMM yyyy HH:mm:ss Z"));

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

    @Override
    public void flush() {
        // TODO Auto-generated method stub
    }

    @Override
    public void close() throws SecurityException {
        // TODO Auto-generated method stub
    }
    
}
