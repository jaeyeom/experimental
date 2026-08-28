"""Tests for the contacts Database."""

import sys

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


if __name__ == '__main__':
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
