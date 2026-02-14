#!/bin/bash

# Script to update .env file with API keys from pass for Taskmaster.
# See .taskmaster/config.json for provider configuration.

# Check if .env file exists
if [ ! -f .env ]; then
    echo "Creating .env from .env.example..."
    cp .env.example .env
fi

# Function to safely update a key in .env file
update_env_key() {
    local key_name="$1"
    local pass_entry="$2"
    if pass "$pass_entry" >/dev/null 2>&1; then
        echo "Updating $key_name..."
        # Get the value from pass
        local value
        value=$(pass "$pass_entry")
        # Create a temporary file
        local tmpfile
        tmpfile=$(mktemp)
        # Process the .env file line by line
        while IFS= read -r line; do
            if [[ "$line" =~ ^${key_name}= ]]; then
                # Replace this line with the new value
                echo "${key_name}=\"${value}\""
            else
                # Keep the line as is
                echo "$line"
            fi
        done <.env >"$tmpfile"
        # Replace the original file
        mv "$tmpfile" .env
    fi
}

# Update API keys
update_env_key "ANTHROPIC_API_KEY" "api.anthropic.com"
update_env_key "PERPLEXITY_API_KEY" "api.perplexity.ai"
update_env_key "OPENAI_API_KEY" "platform.openai.com"
update_env_key "EXA_API_KEY" "api.exa.com"

echo "Done! API keys have been updated in .env"
