# Generated by the gRPC Python protocol compiler plugin. DO NOT EDIT!
"""Client and server classes corresponding to protobuf-defined services."""
import grpc

import contacts_pb2 as contacts__pb2


class ContactsServiceStub(object):
    """Missing associated documentation comment in .proto file."""

    def __init__(self, channel):
        """Constructor.

        Args:
            channel: A grpc.Channel.
        """
        self.ListContacts = channel.unary_unary(
                '/contacts.ContactsService/ListContacts',
                request_serializer=contacts__pb2.ContactListRequest.SerializeToString,
                response_deserializer=contacts__pb2.ContactListResponse.FromString,
                _registered_method=True)
        self.UpsertContact = channel.unary_unary(
                '/contacts.ContactsService/UpsertContact',
                request_serializer=contacts__pb2.UpsertContactRequest.SerializeToString,
                response_deserializer=contacts__pb2.UpsertContactResponse.FromString,
                _registered_method=True)
        self.DeleteContact = channel.unary_unary(
                '/contacts.ContactsService/DeleteContact',
                request_serializer=contacts__pb2.DeleteContactRequest.SerializeToString,
                response_deserializer=contacts__pb2.DeleteContactResponse.FromString,
                _registered_method=True)


class ContactsServiceServicer(object):
    """Missing associated documentation comment in .proto file."""

    def ListContacts(self, request, context):
        """Missing associated documentation comment in .proto file."""
        context.set_code(grpc.StatusCode.UNIMPLEMENTED)
        context.set_details('Method not implemented!')
        raise NotImplementedError('Method not implemented!')

    def UpsertContact(self, request, context):
        """Missing associated documentation comment in .proto file."""
        context.set_code(grpc.StatusCode.UNIMPLEMENTED)
        context.set_details('Method not implemented!')
        raise NotImplementedError('Method not implemented!')

    def DeleteContact(self, request, context):
        """Missing associated documentation comment in .proto file."""
        context.set_code(grpc.StatusCode.UNIMPLEMENTED)
        context.set_details('Method not implemented!')
        raise NotImplementedError('Method not implemented!')


def add_ContactsServiceServicer_to_server(servicer, server):
    rpc_method_handlers = {
            'ListContacts': grpc.unary_unary_rpc_method_handler(
                    servicer.ListContacts,
                    request_deserializer=contacts__pb2.ContactListRequest.FromString,
                    response_serializer=contacts__pb2.ContactListResponse.SerializeToString,
            ),
            'UpsertContact': grpc.unary_unary_rpc_method_handler(
                    servicer.UpsertContact,
                    request_deserializer=contacts__pb2.UpsertContactRequest.FromString,
                    response_serializer=contacts__pb2.UpsertContactResponse.SerializeToString,
            ),
            'DeleteContact': grpc.unary_unary_rpc_method_handler(
                    servicer.DeleteContact,
                    request_deserializer=contacts__pb2.DeleteContactRequest.FromString,
                    response_serializer=contacts__pb2.DeleteContactResponse.SerializeToString,
            ),
    }
    generic_handler = grpc.method_handlers_generic_handler(
            'contacts.ContactsService', rpc_method_handlers)
    server.add_generic_rpc_handlers((generic_handler,))
    server.add_registered_method_handlers('contacts.ContactsService', rpc_method_handlers)


 # This class is part of an EXPERIMENTAL API.
class ContactsService(object):
    """Missing associated documentation comment in .proto file."""

    @staticmethod
    def ListContacts(request,
            target,
            options=(),
            channel_credentials=None,
            call_credentials=None,
            insecure=False,
            compression=None,
            wait_for_ready=None,
            timeout=None,
            metadata=None):
        return grpc.experimental.unary_unary(
            request,
            target,
            '/contacts.ContactsService/ListContacts',
            contacts__pb2.ContactListRequest.SerializeToString,
            contacts__pb2.ContactListResponse.FromString,
            options,
            channel_credentials,
            insecure,
            call_credentials,
            compression,
            wait_for_ready,
            timeout,
            metadata,
            _registered_method=True)

    @staticmethod
    def UpsertContact(request,
            target,
            options=(),
            channel_credentials=None,
            call_credentials=None,
            insecure=False,
            compression=None,
            wait_for_ready=None,
            timeout=None,
            metadata=None):
        return grpc.experimental.unary_unary(
            request,
            target,
            '/contacts.ContactsService/UpsertContact',
            contacts__pb2.UpsertContactRequest.SerializeToString,
            contacts__pb2.UpsertContactResponse.FromString,
            options,
            channel_credentials,
            insecure,
            call_credentials,
            compression,
            wait_for_ready,
            timeout,
            metadata,
            _registered_method=True)

    @staticmethod
    def DeleteContact(request,
            target,
            options=(),
            channel_credentials=None,
            call_credentials=None,
            insecure=False,
            compression=None,
            wait_for_ready=None,
            timeout=None,
            metadata=None):
        return grpc.experimental.unary_unary(
            request,
            target,
            '/contacts.ContactsService/DeleteContact',
            contacts__pb2.DeleteContactRequest.SerializeToString,
            contacts__pb2.DeleteContactResponse.FromString,
            options,
            channel_credentials,
            insecure,
            call_credentials,
            compression,
            wait_for_ready,
            timeout,
            metadata,
            _registered_method=True)
