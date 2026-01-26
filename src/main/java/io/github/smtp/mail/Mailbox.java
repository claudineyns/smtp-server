package io.github.smtp.mail;

public class Mailbox
{
    private final String fullname;
    private final String user;
    private final String domain;

    public Mailbox(final String fullname, final String user, final String domain)
    {
        this.fullname = fullname;
        this.user = user;
        this.domain = domain;
    }

    public Mailbox(final String user, final String domain)
    {
        this.fullname = "";
        this.user = user;
        this.domain = domain;
    }

    public Mailbox(final String email)
    {
        this.fullname = "";
        this.user = email.substring(0, email.indexOf('@'));
        this.domain = email.substring(email.indexOf('@') + 1);
    }

    public String getFullname()
    {
        return fullname;
    }

    public String getUser()
    {
        return user;
    }

    public String getDomain()
    {
        return domain;
    }

    public String getEmail()
    {
        return user.isBlank() && domain.isBlank() ? "" : (user + "@" + domain);
    }

    public String getFullEmail()
    {
        return this.fullname + " <" + this.getEmail() + ">";
    }

    public boolean is(final String email)
    {
        return getEmail().equals(email);
    }

}
