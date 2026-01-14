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

import io.github.smtp.configs.Configs;
import io.github.smtp.configs.SslConfigs;
import io.github.smtp.configs.UtilConfigs;
import io.github.smtp.utils.AppUtils;
import io.github.smtp.workers.SMTPWorker;
import jakarta.enterprise.context.ApplicationScoped;
import jakarta.inject.Inject;

import java.util.List;
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
public class SMTPAgent {
	@Inject
	Logger logger;

	@Inject
	Configs configs;

	@Inject
	SslConfigs sslConfigs;

	public void start()
	{
		logger.infof("application.server.hostname = %s", fetchServiceHost() );
		logger.infof("application.server.port = %s", getPort() );
		logger.infof("application.server.external-port = %s", getExternalPort() );
		logger.infof("application.server.ssl.port = %s", getSslPort() );
		logger.infof("application.server.ssl.external-port = %s", getSslExternalPort() );
		logger.infof("application.content-folder = %s", fetchContentFolder() );

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
		stopInsecure();
		stopSecure();
	}

	private void stopInsecure()
	{
		try {
			if(this.server != null && !this.server.isClosed()) {
				this.server.close();
			}
		} catch (IOException failure) {
			logger.warnf(failure.getMessage());
		}
	}

	private void stopSecure()
	{
		try {
			if(this.sslServer != null && !this.sslServer.isClosed()) {
				this.sslServer.close();
			}
		} catch (IOException failure) {
			logger.warnf(failure.getMessage());
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

	private String fetchContentFolder()
	{
		return configs.contentFolder().orElseGet(() -> System.getProperty("java.io.tmpdir"));
	}

	static final Integer DEFAULT_SMTP_PORT = 25;

	private String serviceHost;
	private String serviceAddress;

	private List<String> whitelist;

	private String contentFolder;

	private ExecutorService threads = Executors.newVirtualThreadPerTaskExecutor();

	private ServerSocket server;

	private SSLServerSocket sslServer;

	private SSLSocketFactory sslSocketFactory;

	private void mountServers() throws Exception {
        this.serviceHost = fetchServiceHost();

		this.whitelist = AppUtils.listOf
		(
			configs.server().fqdn().flatMap(Configs.Server.Fqdn::whitelist)
		)
		.orElseGet(()-> List.of("localhost"));

		this.contentFolder = fetchContentFolder();

		final var address = InetAddress.getByName(serviceHost);
		final var socketAddress = new InetSocketAddress(address, getPort());

		this.serviceAddress = address.getHostAddress();

		this.server = new ServerSocket();
		this.server.bind(socketAddress);

		final var executorService = Executors.newFixedThreadPool(2);

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

        params.setApplicationProtocols(new String[] {"http/1.1"});
        params.setProtocols(new String[] {"TLSv1.2", "TLSv1.3"});
        params.setUseCipherSuitesOrder(true);

        this.sslServer.setSSLParameters(params);

		executorService.submit(() -> this.startInsecureServer());

		executorService.submit(() -> this.startSecureServer());
	}

	private void startInsecureServer()
	{
		logger.infof(">>> Server started on port %s <<<", getPort());

		while (true) {
			Socket client = null;
			try {
				client = server.accept();
			} catch (IOException failure) {
				logger.warn(failure.getMessage());
				break;
			}

			logger.info("\n");
			logger.trace("--- Server got new connection ---");

			this.threads.submit(
				new SMTPWorker(client, this.serviceAddress, UUID.randomUUID(), whitelist)
					.setHostname(this.serviceHost)
					.setContentFolder(this.contentFolder)
					.setSslSocketFactory(this.sslSocketFactory)
			);
		}

		logger.trace("--- Server not accepting new connections");
	}

	private void startSecureServer()
	{
		logger.infof(">>> TLS Server started on port %s <<<", getSslPort());

		while (true) {
			SSLSocket client = null;
			try {
				client = (SSLSocket) sslServer.accept();
			} catch (IOException failure) {
				logger.warn(failure.getMessage());
				break;
			}

			logger.info("\n");
			logger.trace("--- TLS Server got new connection ---");

			this.threads.submit(
				new SMTPWorker(client, this.serviceAddress, UUID.randomUUID(), whitelist)
					.setHostname(this.serviceHost)
					.setContentFolder(this.contentFolder)
					.setSslSocketFactory(this.sslSocketFactory)
			);
		}

		logger.trace("--- TLS Server not accepting new connections");
	}

}
