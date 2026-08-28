"""Tests for ContactsClient."""

import sys

import pytest
import protovalidate

from gen import contacts_pb2
import client as contacts_client


class FakeStub:
    """Records RPC calls and returns canned responses."""

    def __init__(self) -> None:
        self.calls: list[object] = []
        self.list_response = contacts_pb2.ContactListResponse()
        self.upsert_response = contacts_pb2.UpsertContactResponse()
        self.delete_response = contacts_pb2.DeleteContactResponse()

    def ListContacts(
        self, request: contacts_pb2.ContactListRequest,
    ) -> contacts_pb2.ContactListResponse:
        self.calls.append(request)
        return self.list_response

    def UpsertContact(
        self, request: contacts_pb2.UpsertContactRequest,
    ) -> contacts_pb2.UpsertContactResponse:
        self.calls.append(request)
        return self.upsert_response

    def DeleteContact(
        self, request: contacts_pb2.DeleteContactRequest,
    ) -> contacts_pb2.DeleteContactResponse:
        self.calls.append(request)
        return self.delete_response


def test_list_contacts_calls_stub_when_valid() -> None:
    stub = FakeStub()
    alice = contacts_pb2.Contact(uuid='a', name='Alice')
    stub.list_response = contacts_pb2.ContactListResponse(
        contacts=[alice],
    )
    svc = contacts_client.ContactsClient(
        stub=stub, validate=lambda _req: None,
    )
    got = svc.list_contacts(query='Ali')
    assert list(got) == [alice]
    assert stub.calls[0].query == 'Ali'


def test_list_contacts_skips_stub_when_invalid() -> None:
    stub = FakeStub()

    def fail(_request: object) -> None:
        raise protovalidate.ValidationError('invalid', [])

    svc = contacts_client.ContactsClient(stub=stub, validate=fail)
    with pytest.raises(protovalidate.ValidationError):
        svc.list_contacts()
    assert stub.calls == []


def test_upsert_contact_calls_stub_when_valid() -> None:
    stub = FakeStub()
    stored = contacts_pb2.Contact(uuid='a', name='Alice')
    stub.upsert_response = contacts_pb2.UpsertContactResponse(
        contact=stored,
    )
    svc = contacts_client.ContactsClient(
        stub=stub, validate=lambda _req: None,
    )
    got = svc.upsert_contact(contacts_pb2.Contact(name='Alice'))
    assert got == stored
    assert stub.calls


def test_delete_contact_calls_stub_when_valid() -> None:
    stub = FakeStub()
    stored = contacts_pb2.Contact(uuid='a', name='Alice')
    stub.delete_response = contacts_pb2.DeleteContactResponse(
        contact=stored,
    )
    svc = contacts_client.ContactsClient(
        stub=stub, validate=lambda _req: None,
    )
    got = svc.delete_contact('a')
    assert got == stored


if __name__ == '__main__':
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
