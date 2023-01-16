package io.github.rfc5321.server;

import java.io.IOException;
import java.net.Inet4Address;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.logging.Logger;

import io.github.rfc5321.logging.LoggerService;

import java.util.Collections;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.Executors;

public class SMTPAgent {
	static final SMTPAgent INSTANCE = new SMTPAgent();

	public static void main(String[] args) throws IOException {
		INSTANCE.start();
	}

	private final Logger logger = LoggerService.getLogger(getClass().getSimpleName());

	private ServerSocket server;

	public SMTPAgent start() throws IOException {
		final Thread shutdown = new Thread(() -> {
			this.stopService();
			logger.info("Service terminated.");
		});
		Runtime.getRuntime().addShutdownHook(shutdown);

		Executors.newSingleThreadExecutor().submit(() -> startService());

		return this;
	}

	public void stop() {
		stopService();
	}

	private void startService() {
		try {
			listen();
		} catch (IOException e) {
			stopService();
		}
	}

	private void stopService() {
		try {
			if(this.server != null && !this.server.isClosed()) {
				this.server.close();
			}
		} catch (IOException e) { /***/ }
	}

	private void listen() throws IOException {
        final String serviceHost = Optional
            .ofNullable(System.getenv("SMTP_HOSTNAME"))
            .orElse(Optional
                .ofNullable(System.getProperty("smtp.hostname"))
                .orElse(Optional
					.ofNullable(System.getenv("HOSTNAME"))
					.orElse(Inet4Address.getLocalHost().getHostName()))
        );

		final String servicePort = Optional
				.ofNullable(System.getenv("SMTP_PORT"))
				.orElse(Optional
						.ofNullable(System.getProperty("smtp.port"))
						.orElse("25"));

		final Inet4Address address = (Inet4Address) Inet4Address.getByName(serviceHost);
		final InetSocketAddress socketAddress = new InetSocketAddress(address, Integer.parseInt(servicePort));

		this.server = new ServerSocket();
		this.server.bind(socketAddress);

		logger.info("--- Version 0.0.1 ---");
		logger.info(String.format("Started on host %s and port %s", serviceHost, servicePort));

		while (true) {
			Socket client = null;
			try {
				client = server.accept();
				logger.info("Connection received.");
			} catch (IOException e) {
				break;
			}

			Executors.newSingleThreadExecutor().submit(new SMTPInstance(client, UUID.randomUUID(), Collections.emptyList()));
		}

	}

}
