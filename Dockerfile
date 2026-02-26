FROM swipl:stable

# Set the working directory to /app
WORKDIR /app

# Copy the prolog directory to the container
COPY prolog/ ./prolog/

# Expose port 8000 for the API
EXPOSE 8000

# Start the SWI-Prolog HTTP server securely binding to the Render PORT
CMD ["swipl", "-s", "prolog/server.pl", "-g", "start_server, thread_get_message(_)."]
