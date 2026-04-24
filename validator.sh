#!/usr/bin/env -S bash
if [[ -f "$PWD/scripts/run-validator" ]]; then
  exec "$PWD/scripts/run-validator" "$@"
else
  echo "Validator not found, building..."
  scala --power package "$PWD/scripts/validator.sc" -o "$PWD/scripts/run-validator" && \
    chmod +x "$PWD/scripts/run-validator" && \
    exec "$PWD/scripts/run-validator" "$@"
fi
