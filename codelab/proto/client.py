"""Client implements the gRPC server for the contacts service."""

import grpc
import protovalidate

from gen import contacts_pb2
from gen import contacts_pb2_grpc


class ContactsClient:
    """Contacts client."""

    def __init__(self, channel):
        """Initialize the client."""
        self.stub = contacts_pb2_grpc.ContactsServiceStub(channel)

    def list_contacts(self, query=''):
        """List contacts."""
        request = contacts_pb2.ContactListRequest(query=query)
        try:
            protovalidate.validate(request)
        except protovalidate.ValidationError as e:
            print(e.violations)
            return
        response = self.stub.ListContacts(request)
        return response.contacts

    def upsert_contact(self, contact):
        """Upsert contact."""
        request = contacts_pb2.UpsertContactRequest(contact=contact)
        try:
            protovalidate.validate(request)
        except protovalidate.ValidationError as e:
            print(e.violations)
            return
        response = self.stub.UpsertContact(request)
        return response.contact

    def delete_contact(self, uuid):
        """Delete contact."""
        request = contacts_pb2.DeleteContactRequest(uuid=uuid)
        try:
            protovalidate.validate(request)
        except protovalidate.ValidationError as e:
            print(e.violations)
            return
        response = self.stub.DeleteContact(request)
        return response.contact


def main():
    """Main function."""
    channel = grpc.insecure_channel('localhost:50051')
    client = ContactsClient(channel)
    print('Upsert contacts:')
    alice_contact = contacts_pb2.Contact(name='Alice', email='alice@example.com', phone='123-456-7890')
    alice_contact = client.upsert_contact(alice_contact)
    print(alice_contact)
    bob_contact = contacts_pb2.Contact(name='Bob', email='', phone='222-333-4444')
    bob_contact = client.upsert_contact(bob_contact)
    print(bob_contact)
    # Wrong email fails validation
    wrong_contact = contacts_pb2.Contact(name='Wrong', email='wrong', phone='555-666-7777')
    wrong_contact = client.upsert_contact(wrong_contact)
    print(wrong_contact)
    print('List contacts:')
    contacts = client.list_contacts()
    print(contacts)
    print('List contacts with query:')
    contacts = client.list_contacts(query='Alice')
    print(contacts)
    print('Delete contacts:')
    alice_contact = client.delete_contact(alice_contact.uuid)
    print(alice_contact)
    print('List contacts:')
    contacts = client.list_contacts()
    print(contacts)
    print('Delete contacts:')
    bob_contact = client.delete_contact(bob_contact.uuid)
    print(bob_contact)
    print('List contacts:')
    contacts = client.list_contacts()
    print(contacts)


if __name__ == '__main__':
    main()
