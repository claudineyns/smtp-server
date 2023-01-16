package io.github.rfc3507;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.logging.LogManager;
import java.util.logging.Logger;
import java.util.Collections;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executors;

public class SMTPAgent {
	static final SMTPAgent INSTANCE = new SMTPAgent();

	static {
		LogManager.getLogManager().reset();
		Logger rootLogger = LogManager.getLogManager().getLogger("");
		rootLogger.addHandler(new LoggerService());
	}

	public static void main(String[] args) throws IOException {
		INSTANCE.start();
	}

	private final Logger logger = LogManager.getLogManager().getLogger("");

	private ServerSocket server;

	public void start() throws IOException {
		final Thread shutdown = new Thread(() -> {
			try {
				if (!server.isClosed()) {
					server.close();
				}
			} catch (IOException e) {
				/***/
			}
			logger.info("Service terminated.");
		});
		Runtime.getRuntime().addShutdownHook(shutdown);

		Executors.newSingleThreadExecutor().submit(() -> startService());
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
			server.close();
		} catch (IOException e) {
			/***/
		}
	}

	private void listen() throws IOException {

		final String servicePort = Optional
				.ofNullable(System.getenv("SMTP_PORT"))
				.orElse(Optional
						.ofNullable(System.getProperty("smtp.port"))
						.orElse("25"));

		this.server = new ServerSocket(Integer.parseInt(servicePort));

		logger.info("Started on port " + servicePort);

		while (true) {
			Socket client = null;
			try {
				client = server.accept();
				logger.info("Connection received.");
			} catch (IOException e) {
				break;
			}

			CompletableFuture.runAsync(new SMTPInstance(client, UUID.randomUUID(), Collections.emptyList()));
		}

	}

}