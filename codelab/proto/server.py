"""Server implements the gRPC server for the contacts service."""

import logging
import uuid
from collections.abc import Callable, MutableMapping
from concurrent import futures

import grpc
import protovalidate

from gen import contacts_pb2, contacts_pb2_grpc

logger = logging.getLogger(__name__)


class Database:
    """In-memory contact store."""

    def __init__(
        self,
        kv_storage: MutableMapping[str, bytes],
        *,
        new_id: Callable[[], str] | None = None,
    ) -> None:
        """Initialize the database.

        Args:
            kv_storage: Mapping from contact uuid to serialized Contact.
            new_id: Factory for new uuids. Defaults to uuid4 strings.
        """
        self.contacts = kv_storage
        self._new_id = (
            new_id if new_id is not None else lambda: str(uuid.uuid4())
        )

    def list_contacts(
        self,
        query: str = '',
    ) -> list[contacts_pb2.Contact]:
        """List contacts whose name, email, or phone contains query."""
        decoded = (
            contacts_pb2.Contact.FromString(raw)
            for raw in self.contacts.values()
        )
        return [
            contact
            for contact in decoded
            if (
                query in contact.name
                or query in contact.email
                or query in contact.phone
            )
        ]

    def upsert_contact(
        self,
        contact: contacts_pb2.Contact,
    ) -> contacts_pb2.Contact:
        """Insert or update a contact, assigning uuid when missing."""
        if not contact.uuid:
            contact.uuid = self._new_id()
        self.contacts[contact.uuid] = contact.SerializeToString()
        return contacts_pb2.Contact.FromString(
            self.contacts[contact.uuid],
        )

    def delete_contact(
        self,
        contact_uuid: str,
    ) -> contacts_pb2.Contact:
        """Delete a contact by uuid.

        Raises:
            KeyError: If contact_uuid is not in the store.
        """
        return contacts_pb2.Contact.FromString(
            self.contacts.pop(contact_uuid),
        )


class ContactsService(contacts_pb2_grpc.ContactsServiceServicer):
    """Contacts service implementation."""

    def __init__(self, db: Database) -> None:
        """Initialize the service."""
        self.db = db

    def ListContacts(
        self,
        request: contacts_pb2.ContactListRequest,
        context: grpc.ServicerContext,
    ) -> contacts_pb2.ContactListResponse:
        """List contacts."""
        return contacts_pb2.ContactListResponse(
            contacts=self.db.list_contacts(request.query),
        )

    def UpsertContact(
        self,
        request: contacts_pb2.UpsertContactRequest,
        context: grpc.ServicerContext,
    ) -> contacts_pb2.UpsertContactResponse:
        """Upsert contact."""
        return contacts_pb2.UpsertContactResponse(
            contact=self.db.upsert_contact(request.contact),
        )

    def DeleteContact(
        self,
        request: contacts_pb2.DeleteContactRequest,
        context: grpc.ServicerContext,
    ) -> contacts_pb2.DeleteContactResponse:
        """Delete contact."""
        return contacts_pb2.DeleteContactResponse(
            contact=self.db.delete_contact(request.uuid),
        )


class ValidationInterceptor(grpc.ServerInterceptor):
    """Validate unary requests with protovalidate before the handler."""

    def __init__(
        self,
        *,
        validate: Callable[[object], None] | None = None,
    ) -> None:
        """Initialize the interceptor.

        Args:
            validate: Request validator. Defaults to protovalidate.validate.
        """
        self._validate = (
            validate if validate is not None else protovalidate.validate
        )

    def intercept_service(
        self,
        continuation: Callable[
            [object],
            grpc.RpcMethodHandler | None,
        ],
        handler_call_details: object,
    ) -> grpc.RpcMethodHandler | None:
        """Wrap unary handlers with request validation."""
        handler = continuation(handler_call_details)
        if handler is None or handler.unary_unary is None:
            return handler

        inner = handler.unary_unary

        def unary_unary(
            request: object,
            context: grpc.ServicerContext,
        ) -> object:
            try:
                self._validate(request)
            except protovalidate.ValidationError as err:
                context.abort(
                    grpc.StatusCode.INVALID_ARGUMENT,
                    str(err.violations),
                )
            return inner(request, context)

        return grpc.unary_unary_rpc_method_handler(
            unary_unary,
            request_deserializer=handler.request_deserializer,
            response_serializer=handler.response_serializer,
        )


def main() -> None:
    """Start the contacts gRPC server on port 50051."""
    db = Database({})
    grpc_server = grpc.server(
        futures.ThreadPoolExecutor(),
        interceptors=[ValidationInterceptor()],
    )
    contacts_pb2_grpc.add_ContactsServiceServicer_to_server(
        ContactsService(db),
        grpc_server,
    )
    grpc_server.add_insecure_port('[::]:50051')
    grpc_server.start()
    grpc_server.wait_for_termination()


if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO)
    main()
