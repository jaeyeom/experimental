"""Tests for the contacts Database."""

import sys

import grpc
import protovalidate
import pytest

from gen import contacts_pb2
import server


def _contact(
    *,
    uuid: str = '',
    name: str = '',
    email: str = '',
    phone: str = '',
) -> contacts_pb2.Contact:
    return contacts_pb2.Contact(
        uuid=uuid, name=name, email=email, phone=phone,
    )


def _store(*contacts: contacts_pb2.Contact) -> dict[str, bytes]:
    return {c.uuid: c.SerializeToString() for c in contacts}


@pytest.mark.parametrize(
    ('query', 'want_names'),
    [
        ('', ['Alice', 'Bob']),
        ('Ali', ['Alice']),
        ('example.com', ['Alice']),
        ('222', ['Bob']),
        ('zzz', []),
    ],
)
def test_list_contacts_filters_by_query(
    query: str, want_names: list[str],
) -> None:
    db = server.Database(
        _store(
            _contact(uuid='a', name='Alice', email='alice@example.com'),
            _contact(uuid='b', name='Bob', phone='222-333-4444'),
        ),
    )
    got = [c.name for c in db.list_contacts(query)]
    assert got == want_names


def test_upsert_assigns_id_when_missing() -> None:
    db = server.Database({}, new_id=lambda: 'fixed-id')
    got = db.upsert_contact(_contact(name='Carol'))
    assert got.uuid == 'fixed-id'
    assert 'fixed-id' in db.contacts


def test_upsert_keeps_existing_id() -> None:
    db = server.Database({}, new_id=lambda: 'should-not-run')
    got = db.upsert_contact(_contact(uuid='kept-id', name='Dave'))
    assert got.uuid == 'kept-id'


def test_delete_returns_contact() -> None:
    stored = _contact(uuid='a', name='Alice')
    db = server.Database(_store(stored))
    got = db.delete_contact('a')
    assert got.name == 'Alice'
    assert db.contacts == {}


def test_delete_missing_uuid_raises_key_error() -> None:
    db = server.Database({})
    with pytest.raises(KeyError):
        db.delete_contact('missing')


class _AbortError(Exception):
    def __init__(self, code: grpc.StatusCode, details: str) -> None:
        super().__init__(details)
        self.code = code
        self.details = details


class _FakeContext:
    def abort(self, code: grpc.StatusCode, details: str) -> None:
        raise _AbortError(code, details)


class _FakeHandler:
    def __init__(self, unary_unary: object | None) -> None:
        self.unary_unary = unary_unary
        self.request_deserializer = None
        self.response_serializer = None


def test_interceptor_aborts_on_validation_error() -> None:
    def fail(_request: object) -> None:
        raise protovalidate.ValidationError('invalid', [])

    interceptor = server.ValidationInterceptor(validate=fail)
    inner_called = []

    def inner(request: object, context: object) -> str:
        inner_called.append(request)
        return 'ok'

    handler = interceptor.intercept_service(
        lambda _details: _FakeHandler(inner),
        handler_call_details=None,
    )
    context = _FakeContext()
    with pytest.raises(_AbortError) as caught:
        handler.unary_unary(object(), context)
    assert caught.value.code == grpc.StatusCode.INVALID_ARGUMENT
    assert inner_called == []


def test_interceptor_delegates_when_valid() -> None:
    interceptor = server.ValidationInterceptor(validate=lambda _r: None)
    handler = interceptor.intercept_service(
        lambda _details: _FakeHandler(lambda _req, _ctx: 'ok'),
        handler_call_details=None,
    )
    assert handler.unary_unary(object(), _FakeContext()) == 'ok'


def test_interceptor_passes_through_non_unary() -> None:
    original = _FakeHandler(unary_unary=None)
    interceptor = server.ValidationInterceptor(validate=lambda _r: None)
    got = interceptor.intercept_service(
        lambda _details: original,
        handler_call_details=None,
    )
    assert got is original


if __name__ == '__main__':
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
