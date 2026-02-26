FROM swipl:stable

# Set the working directory to /app
WORKDIR /app

# Copy the prolog directory to the container
COPY prolog/ ./prolog/

# Expose port 8000 for the API
EXPOSE 8000

# Start the SWI-Prolog HTTP server
CMD ["swipl", "-s", "prolog/server.pl", "-g", "server(8000), thread_get_message(_)."]
