RUN chmod -R +x /usr/local/server/helpers

EXPOSE 3000

# Override base-image defaults (for example SIGQUIT) for a common stop contract.
STOPSIGNAL SIGTERM
