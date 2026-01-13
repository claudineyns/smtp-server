package io.github.smtp.server;

import java.io.IOException;

import java.net.Inet4Address;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.UnknownHostException;

import org.jboss.logging.Logger;

import io.github.smtp.configs.Configs;
import io.github.smtp.configs.UtilConfigs;
import io.github.smtp.utils.AppUtils;
import io.github.smtp.workers.SMTPWorker;
import jakarta.enterprise.context.ApplicationScoped;
import jakarta.inject.Inject;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

@ApplicationScoped
public class SMTPAgent {
	@Inject
	Logger logger;

	@Inject
	Configs configs;

	private ServerSocket server;

	public void start()
	{
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
		} catch (IOException e) {
			stopServer();
		}
	}

	private void stopServer()
	{
		try {
			if(this.server != null && !this.server.isClosed()) {
				this.server.close();
			}
		} catch (IOException e) {
			logger.warnf(e.getMessage());
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

	static final Integer DEFAULT_SMTP_PORT = 25;

	private String serviceHost;

	private Integer servicePort;

	private List<String> whitelist;

	private String contentFolder;

	private ExecutorService threads = Executors.newVirtualThreadPerTaskExecutor();

	private void mountServers() throws IOException {
        this.serviceHost = configs.server().hostname()
			.or( () -> UtilConfigs.OPTIONAL_HOSTNAME )
			.orElseGet(this::getLocalhostName);

		this.servicePort = configs.server().port().orElse(DEFAULT_SMTP_PORT);

		this.whitelist = AppUtils.listOf
		(
			configs.server().fqdn().flatMap(Configs.Server.Fqdn::whitelist)
		)
		.orElseGet(()-> List.of("localhost"));

		this.contentFolder = configs
			.contentFolder()
			.orElseGet(() -> System.getProperty("java.io.tmpdir"));

		final var address = InetAddress.getByName(serviceHost);
		final var socketAddress = new InetSocketAddress(address, servicePort);

		this.server = new ServerSocket();
		this.server.bind(socketAddress);

		final var executorService = Executors.newFixedThreadPool(2);

		executorService.submit(() -> this.startInsecureServer());
	}

	private void startInsecureServer()
	{
		logger.infof(">>> Server started on host %s and port %s <<<", serviceHost, servicePort);

		while (true) {
			Socket client = null;
			try {
				client = server.accept();
				logger.trace("--- Server got new connection ---");
			} catch (IOException e) {
				break;
			}

			this.threads.submit(
				new SMTPWorker(client, UUID.randomUUID(), whitelist)
					.setHostname(this.serviceHost)
					.setContentFolder(this.contentFolder)
			);
		}

		logger.trace("--- Server not accepting new connections");
	}

}
