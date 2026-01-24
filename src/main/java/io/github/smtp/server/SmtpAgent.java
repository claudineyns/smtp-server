package io.github.smtp.server;

import java.io.IOException;

import java.net.Inet4Address;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.URI;
import java.net.UnknownHostException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.KeyStore;
import org.jboss.logging.Logger;

import io.github.smtp.application.Mode;
import io.github.smtp.configs.Configs;
import io.github.smtp.configs.SslConfigs;
import io.github.smtp.configs.SubmissionConfigs;
import io.github.smtp.configs.UtilConfigs;
import io.github.smtp.workers.SmtpWorker;

import static io.github.smtp.utils.AppUtils.listOf;

import jakarta.enterprise.context.ApplicationScoped;
import jakarta.inject.Inject;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLServerSocket;
import javax.net.ssl.SSLSessionContext;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.SSLSocketFactory;

@ApplicationScoped
public class SmtpAgent {
	@Inject
	Logger logger;

	@Inject
	Configs configs;

	@Inject
	SslConfigs sslConfigs;

	@Inject
	SubmissionConfigs submissionConfigs;

    private final Map<String, String> addressHostCache = Collections.synchronizedMap(
        new LinkedHashMap<String, String>(1024, 0.75f, true) {
            @Override
            protected boolean removeEldestEntry(Map.Entry<String, String> eldest) {
                return size() > 1000;
            }
        }
    );

	public void start()
	{
		logger.infof("application.mode = %s", configs.mode() );
		logger.infof("application.content-folder = %s", fetchContentFolder() );
		configs.forbiddenHostWords().ifPresent(l -> logger.infof("application.forbidden-host-words = %s", l));
		logger.infof("application.server.hostname = %s", fetchServiceHost() );
		configs.server().fqdnWhitelist().ifPresent(l -> logger.infof("application.server.fqdn-whitelist = %s", l));
		logger.infof("application.server.port = %s", getPort() );
		logger.infof("application.server.external-port = %s", getExternalPort() );
		logger.infof("application.server.ssl.port = %s", getSslPort() );
		logger.infof("application.server.ssl.external-port = %s", getSslExternalPort() );
		logger.infof("application.server.submission.port = %s", getSubmissionPort() );
		logger.infof("application.server.submission.external-port = %s", getSubmissionExternalPort() );

		Executors.newSingleThreadExecutor().submit(() -> startServer());
	}

	public void stop()
	{
		stopServer();
	}

	private void startServer()
	{
		try {
			mountServers();
		} catch (Exception failure) {
			logger.error(failure.getMessage());
			stopServer();
		}
	}

	private void stopServer()
	{
		stopSmtp();
		stopSubmission();
		stopSecureSubmission();
	}

	private void stopSmtp()
	{
		try {
			if(this.server != null && !this.server.isClosed()) {
				this.server.close();
			}
		} catch (IOException failure) {
			logger.warnf("[stopSmtp] %s", failure.getMessage());
		}
	}

	private void stopSubmission()
	{
		try {
			if(this.submissionServer != null && !this.submissionServer.isClosed()) {
				this.submissionServer.close();
			}
		} catch (IOException failure) {
			logger.warnf("[stopSubmission] %s", failure.getMessage());
		}
	}

	private void stopSecureSubmission()
	{
		try {
			if(this.sslServer != null && !this.sslServer.isClosed()) {
				this.sslServer.close();
			}
		} catch (IOException failure) {
			logger.warnf("[stopSecureSubmission] %s", failure.getMessage());
		}
	}

	private String getLocalhostName()
	{
		try
		{
			return Inet4Address.getLocalHost().getHostName();
		} catch(UnknownHostException failure)
		{
			throw new IllegalStateException(failure);
		}
	}

	private String fetchServiceHost()
	{
		return configs.server().hostname()
			.or( () -> UtilConfigs.OPTIONAL_HOSTNAME )
			.orElseGet(this::getLocalhostName);
	}

	private Integer getExternalPort()
	{
		return configs.server().externalPort().orElseGet(this::getPort);
	}

	private Integer getPort()
	{
		return configs.server().port();
	}

	private Integer getSslExternalPort()
	{
		return sslConfigs.externalPort().orElseGet(this::getSslPort);
	}

	private Integer getSslPort()
	{
		return sslConfigs.port();
	}

	private Integer getSubmissionExternalPort()
	{
		return submissionConfigs.externalPort().orElseGet(this::getSubmissionPort);
	}

	private Integer getSubmissionPort()
	{
		return submissionConfigs.port();
	}

	private String fetchContentFolder()
	{
		return configs.contentFolder().orElseGet(() -> System.getProperty("java.io.tmpdir"));
	}

	private Mode getMode()
	{
		return configs.mode();
	}

	private List<String> fetchForbiddenHostWords()
	{
		return listOf(configs.forbiddenHostWords()).orElseGet(List::of);
	}

	private Optional<String> fetchForbiddenHostMessage()
	{
		return configs.forbiddenHostMessage();
	}

	static final Integer DEFAULT_SMTP_PORT = 25;

	private String serviceHost;
	private String serviceAddress;

	private List<String> whitelist;

	private String contentFolder;

	private ExecutorService threads = Executors.newVirtualThreadPerTaskExecutor();

	private ServerSocket server;

	private ServerSocket submissionServer;

	private SSLServerSocket sslServer;

	private SSLSocketFactory sslSocketFactory;

	private void mountServers() throws Exception {
        this.serviceHost = fetchServiceHost();

		this.whitelist = listOf(configs.server().fqdnWhitelist())
			.orElseGet(()-> List.of("localhost"));

		this.contentFolder = fetchContentFolder();

		final var address = InetAddress.getByName(serviceHost);
		this.serviceAddress = address.getHostAddress();

		final var socketAddress = new InetSocketAddress(address, getPort());
		this.server = new ServerSocket();
		this.server.bind(socketAddress);

		final var submissionSocketAddress = new InetSocketAddress(address, getSubmissionPort());
		this.submissionServer = new ServerSocket();
		this.submissionServer.bind(submissionSocketAddress);

        // KeyManagerFactory

        final char[] keystorepass = sslConfigs
            .keystore()
            .storepass()
            .toCharArray();

		final String keystoreResource = sslConfigs
			.keystore()
			.resource();

		final Path pathResource = keystoreResource.startsWith("file:")
			? Path.of(URI.create(keystoreResource))
			: Path.of(keystoreResource);

        final KeyStore keyStore = KeyStore.getInstance("PKCS12");
		try (var stream = Files.newInputStream(pathResource)) {
        	keyStore.load(stream, keystorepass);
    	}

        final KeyManagerFactory keyManagerFactory = KeyManagerFactory.getInstance("PKIX", "SunJSSE");
        keyManagerFactory.init(keyStore, keystorepass);

        // set up the SSL Context
        final SSLContext sslContext = SSLContext.getInstance("TLS");

        sslContext.init(keyManagerFactory.getKeyManagers(), null, null);

        final SSLSessionContext sessionContext = sslContext.getServerSessionContext();

        // Define how long (seconds) client can use previous handshake
        // 24h is a common value for modern web applications
        sessionContext.setSessionTimeout(86400);

        // How many sessions to keep in RAM memory
        sessionContext.setSessionCacheSize(10000);

		this.sslSocketFactory = sslContext.getSocketFactory();

        this.sslServer = (SSLServerSocket) sslContext
			.getServerSocketFactory()
			.createServerSocket(getSslPort());

        final var params = this.sslServer.getSSLParameters();

        params.setApplicationProtocols(new String[] {"smtp"});
        params.setProtocols(new String[] {"TLSv1.2", "TLSv1.3"});
        params.setUseCipherSuitesOrder(true);

        this.sslServer.setSSLParameters(params);

		Executors.newSingleThreadExecutor().submit(() -> this.startSmtp());
		Executors.newSingleThreadExecutor().submit(() -> this.startSubmission());
		Executors.newSingleThreadExecutor().submit(() -> this.startSecureSubmission());
	}

	private void startSmtp()
	{
		logger.infof(">>> [SMTP] started on port %s <<<", getPort());

		while (true) {
			Socket client = null;
			try {
				client = server.accept();
			} catch (IOException failure) {
				break;
			}

			logger.info("\n");
			logger.trace("--- [SMTP] got new connection ---");

			this.threads.submit(
				new SmtpWorker(client, ServerMode.SMTP, this.serviceAddress, UUID.randomUUID(), whitelist)
					.setHostname(this.serviceHost)
					.setContentFolder(this.contentFolder)
					.setSslSocketFactory(this.sslSocketFactory)
					.setMode(getMode())
					.setForbiddenHostConfig(fetchForbiddenHostWords(), fetchForbiddenHostMessage())
					.setAddressHostCache(addressHostCache)
			);
		}

		logger.trace("--- [SMTP] not accepting new connections");
	}

	private void startSubmission()
	{
		logger.infof(">>> [Submission] started on port %s <<<", getSubmissionPort());

		while (true) {
			Socket client = null;
			try {
				client = submissionServer.accept();
			} catch (IOException failure) {
				break;
			}

			logger.info("\n");
			logger.trace("--- [Submission] got new connection ---");

			this.threads.submit(
				new SmtpWorker(client, ServerMode.SUBMISSION, this.serviceAddress, UUID.randomUUID(), whitelist)
					.setHostname(this.serviceHost)
					.setContentFolder(this.contentFolder)
					.setSslSocketFactory(this.sslSocketFactory)
					.setMode(getMode())
					.setForbiddenHostConfig(fetchForbiddenHostWords(), fetchForbiddenHostMessage())
					.setAddressHostCache(addressHostCache)
			);
		}

		logger.trace("--- [Submission] not accepting new connections");
	}

	private void startSecureSubmission()
	{
		logger.infof(">>> [SSL Submission] started on port %s <<<", getSslPort());

		while (true) {
			SSLSocket client = null;
			try {
				client = (SSLSocket) sslServer.accept();
			} catch (IOException failure) {
				break;
			}

			logger.info("\n");
			logger.trace("--- [SSL Submission] got new connection ---");

			this.threads.submit(
				new SmtpWorker(client, ServerMode.SECURE_SUBMISSION, this.serviceAddress, UUID.randomUUID(), whitelist)
					.setHostname(this.serviceHost)
					.setContentFolder(this.contentFolder)
					.setSslSocketFactory(this.sslSocketFactory)
					.setMode(getMode())
					.setForbiddenHostConfig(fetchForbiddenHostWords(), fetchForbiddenHostMessage())
					.setAddressHostCache(addressHostCache)
			);
		}

		logger.trace("--- [SSL Submission] not accepting new connections");
	}

}
