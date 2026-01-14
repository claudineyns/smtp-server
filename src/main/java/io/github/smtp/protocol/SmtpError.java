package io.github.smtp.protocol;

// https://www.iana.org/assignments/smtp-enhanced-status-codes/smtp-enhanced-status-codes.xhtml
public enum SmtpError {
    TLS_NOT_AVAILABLE("454", "4.7.0", "TLS not available due to temporary reason"),
    INVALID_CREDENTIALS("535", "5.7.8", "Authentication credentials invalid"),
    MAILBOX_MISSING("501", "5.1.1", "Please, provide a mailbox"),
    INVALID_MAILING_LIST_AS_USER("550", "5.7.1", "That is a mailing list, not a user"),
    INVALID_USER_AS_MAILING_LIST("550", "5.7.1", "That is a user name, not a mailing list"),
    AUTHENTICATION_REQUIRED("530", "5.7.0", "Authentication required"),
    RECIPIENTS_MISSING("554", "5.1.0", "No valid recipients"),
    SENDER_MISSING("554", "5.1.0", "Please, identify yourself"),
    DESTINATION_MISSING("554", "5.1.0", "Please, specify a destination mailbox"),
    SENDER_ALREADY_SPECIFIED("503", "5.5.1", "Sender already specified"),
    UNAVAILABLE("421", "4.0.0", "Service not available, closing transmission channel"),
    INVALID_COMMAND("500", "5.5.1", "Invalid command"),
    INTRODUCTION_MISSING("503", "5.5.1", "Please, introduce yourself"),
    SECURITY_POLICY("550", "5.7.1", "Security policy"),
    MESSAGE_TOO_BIG("552", "5.3.4", "Message size exceeds fixed limit"),
    NEED_STARTTLS("530", "5.7.0", "Must issue a STARTTLS command first"),
    TLS_ALREADY_ACTIVE("503", "5.5.1", "TLS already active")
    ;

    private String code;
    private String extended;
    private String message;

    private SmtpError(final String code, final String extended, final String message)
    {
        this.code = code;
        this.extended = extended;
        this.message = message;
    }

    private String code()
    {
        return this.code;
    }

    private String extended()
    {
        return this.extended;
    }

    private String message()
    {
        return this.message;
    }

    public String toString()
    {
        return String.format("%s %s %s", code(), extended(), message());
    }

    public String withHost(final String host)
    {
        return String.format("%s %s %s %s", code(), extended(), host, message());
    }    
}
