# Makefile using buf to generate Python code from contacts.proto

PROTOS := $(wildcard *.proto)
# Replace .proto with _pb2.py under gen/ directory like gen/contacts_pb2.py for contacts.proto
PY_PROTOS := $(PROTOS:%.proto=gen/%_pb2.py)
PY_GRPCS := $(PROTOS:%.proto=gen/%_pb2_grpc.py)

buf.lock:
	buf dep update

$(PY_PROTOS) $(PY_GRPCS) &: $(PROTOS) buf.lock
	buf generate --include-imports

generate: $(PY_PROTOS) $(PY_GRPCS)

run-server: generate
	PYTHONPATH=./gen python3 -m server

run-client: generate
	PYTHONPATH=./gen python3 -m client

.PHONY: generate run-server run-client
