"""Server implements the gRPC server for the contacts service."""

import uuid
from concurrent import futures

import protovalidate
import grpc

from gen import contacts_pb2, contacts_pb2_grpc


class Database:
    """Database abstraction."""

    def __init__(self, kv_storage):
        """Initialize the database."""
        self.contacts = kv_storage

    def list_contacts(self, query=''):
        """List contacts."""
        contacts_bytes = (
            contacts_pb2.Contact.FromString(contact)
            for contact in self.contacts.values()
        )
        return [
            c
            for c in contacts_bytes
            if query in c.name or query in c.email or query in c.phone
        ]

    def upsert_contact(self, contact):
        """Upsert contact."""
        if not contact.uuid:
            contact.uuid = str(uuid.uuid4())
        self.contacts[contact.uuid] = contact.SerializeToString()
        return contacts_pb2.Contact.FromString(self.contacts[contact.uuid])

    def delete_contact(self, uuid):
        """Delete contact."""
        return contacts_pb2.Contact.FromString(self.contacts.pop(uuid))


class ContactsService(contacts_pb2_grpc.ContactsServiceServicer):
    """Contacts service implementation."""

    def __init__(self, db):
        """Initialize the service."""
        self.db = db

    def ListContacts(self, request, context):
        """List contacts."""
        return contacts_pb2.ContactListResponse(
            contacts=self.db.list_contacts(request.query),
        )

    def UpsertContact(self, request, context):
        """Upsert contact."""
        return contacts_pb2.UpsertContactResponse(
            contact=self.db.upsert_contact(request.contact),
        )

    def DeleteContact(self, request, context):
        """Delete contact."""
        return contacts_pb2.DeleteContactResponse(
            contact=self.db.delete_contact(request.uuid),
        )


class ValidationInterceptor(grpc.ServerInterceptor):
    """Validation interceptor that calls protovalidate.validate."""

    def intercept_service(self, continuation, handler_call_details):
        """Intercept the service."""
        request = continuation(handler_call_details)
        print('ValidationInterceptor:', request, handler_call_details)
        try:
            protovalidate.validate(request)
        except protovalidate.ValidationError as e:
            raise grpc.RpcError(
                grpc.StatusCode.INVALID_ARGUMENT,
                str(e.violations),
            )
        return request


def main():
    """Main function."""
    db = Database({})
    server = grpc.server(futures.ThreadPoolExecutor())
    contacts_pb2_grpc.add_ContactsServiceServicer_to_server(
        ContactsService(db),
        server,
    )
    server.add_insecure_port('[::]:50051')
    server.start()
    server.wait_for_termination()


if __name__ == '__main__':
    main()
