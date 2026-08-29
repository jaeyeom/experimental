"""Client for the contacts gRPC service."""

import logging
from collections.abc import Callable
from typing import Protocol

import grpc
import protovalidate

from gen import contacts_pb2, contacts_pb2_grpc

logger = logging.getLogger(__name__)


class ContactsStub(Protocol):
    """gRPC stub surface used by ContactsClient."""

    def ListContacts(
        self,
        request: contacts_pb2.ContactListRequest,
    ) -> contacts_pb2.ContactListResponse:
        """Call ListContacts."""
        ...

    def UpsertContact(
        self,
        request: contacts_pb2.UpsertContactRequest,
    ) -> contacts_pb2.UpsertContactResponse:
        """Call UpsertContact."""
        ...

    def DeleteContact(
        self,
        request: contacts_pb2.DeleteContactRequest,
    ) -> contacts_pb2.DeleteContactResponse:
        """Call DeleteContact."""
        ...


class ContactsClient:
    """Contacts client."""

    def __init__(
        self,
        channel: grpc.Channel | None = None,
        *,
        stub: ContactsStub | None = None,
        validate: Callable[[object], None] | None = None,
    ) -> None:
        """Initialize the client.

        Args:
            channel: Used to build a stub when stub is omitted.
            stub: Prebuilt stub for tests.
            validate: Request validator. Defaults to protovalidate.validate.

        Raises:
            ValueError: If neither channel nor stub is provided.
        """
        if stub is not None:
            self.stub = stub
        elif channel is not None:
            self.stub = contacts_pb2_grpc.ContactsServiceStub(channel)
        else:
            raise ValueError('channel or stub is required')
        self._validate = (
            validate if validate is not None else protovalidate.validate
        )

    def list_contacts(
        self,
        query: str = '',
    ) -> list[contacts_pb2.Contact]:
        """List contacts matching query."""
        request = contacts_pb2.ContactListRequest(query=query)
        self._validate(request)
        return list(self.stub.ListContacts(request).contacts)

    def upsert_contact(
        self,
        contact: contacts_pb2.Contact,
    ) -> contacts_pb2.Contact:
        """Insert or update a contact."""
        request = contacts_pb2.UpsertContactRequest(contact=contact)
        self._validate(request)
        return self.stub.UpsertContact(request).contact

    def delete_contact(
        self,
        contact_uuid: str,
    ) -> contacts_pb2.Contact:
        """Delete a contact by uuid."""
        request = contacts_pb2.DeleteContactRequest(uuid=contact_uuid)
        self._validate(request)
        return self.stub.DeleteContact(request).contact


def main() -> None:
    """Run a demo client against localhost:50051."""
    channel = grpc.insecure_channel('localhost:50051')
    svc = ContactsClient(channel)
    print('Upsert contacts:')
    try:
        alice_contact = svc.upsert_contact(
            contacts_pb2.Contact(
                name='Alice',
                email='alice@example.com',
                phone='123-456-7890',
            ),
        )
        print(alice_contact)
    except protovalidate.ValidationError as err:
        logger.error('upsert Alice failed: %s', err.violations)
        print(err.violations)
        alice_contact = None

    try:
        bob_contact = svc.upsert_contact(
            contacts_pb2.Contact(
                name='Bob',
                email='',
                phone='222-333-4444',
            ),
        )
        print(bob_contact)
    except protovalidate.ValidationError as err:
        logger.error('upsert Bob failed: %s', err.violations)
        print(err.violations)
        bob_contact = None

    # Wrong email fails validation
    try:
        wrong_contact = svc.upsert_contact(
            contacts_pb2.Contact(
                name='Wrong',
                email='wrong',
                phone='555-666-7777',
            ),
        )
        print(wrong_contact)
    except protovalidate.ValidationError as err:
        print(err.violations)

    print('List contacts:')
    print(svc.list_contacts())
    print('List contacts with query:')
    print(svc.list_contacts(query='Alice'))
    if alice_contact is not None:
        print('Delete contacts:')
        print(svc.delete_contact(alice_contact.uuid))
    print('List contacts:')
    print(svc.list_contacts())
    if bob_contact is not None:
        print('Delete contacts:')
        print(svc.delete_contact(bob_contact.uuid))
    print('List contacts:')
    print(svc.list_contacts())


if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO)
    main()
