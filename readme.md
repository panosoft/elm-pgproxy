# Postgres Authenticating Proxy Service in Elm

> This is a Proxy Service for the Elm [Postgres Effects Manager](https://github.com/panosoft/elm-postgres).

> This proxy service allows the Postgres Effects Manager to be used on the **client**.

> The Postgres Effects Manager's native code when run on the client will delegate to a Proxy Service as was specified in its `clientSideConfig` call. This Proxy Service can be used directly or as a reference implementation.

> This module is written as a **service** meaning that it must be housed in a server. See [Test code](#test-code) below for more info.

> This implementation is an authenticating proxy service. It delegates authentication for each request to the server application. The authentication function is injected into the PGProxy service. Once authenticated, it honors the request.

> It's built with the same Postgres Effects Manager as the client except it uses the server native code to make calls to the DB on behalf of the client.

> Communication between client and server uses Websockets using the client library [elm-websocket-browser](https://github.com/panosoft/elm-websocket-browser) and the server library [elm-websocket-server](https://github.com/panosoft/elm-websocket-server)

## Efficiencies

As of version 4.0.0, PGProxy supports Listen Connection Sharing. Since queries and writes to and from the database from clients can be done over short-lived connections, there is no need to share those connections. Since connection pooling is supported by the Postgres Effects Manager, short-lived connections can be employed efficiently.

However, this is not the case with Listens. Those connection must be long-lived and could create too many connections from all of the clients of the database.

Therefore, PGProxy now supports Connection Sharing for Listens. This is done internally and doesn't require any additional Client logic, but does inforce a restriction. Listens MUST have their own connections. If you want to reuse a Listen connection, you may, but only after you stop listening, i.e. get a successful Unlisten event.

## Protocol

### Requests

Requests are JSON and of the format:

```js
{
	"sessionId": "1f137f5f-43ec-4393-b5e8-bf195015e697",
	"requestId": "13",
	// the rest of the keys for the Service's request
}
```

The only expectation for `sessionId` is that it's a `String`. This information is beneficial for authentication. The only requirement is that the authentication function of the `PGProxy` module knows how to interpret this string to authenticate the request.

If you examine the [Postgres Effects Manager](https://github.com/elm-postgres), you'll notice the `clientSideConfig` function takes a `json` parameter which is a JSON encoded string that gets merged with all outgoing requests. This is how the `sessionId` is attached to the standard request.

This was done to allow maximum flexibility for users of this module. Here's an example of how to configure Postgres on the client:

```elm
Postgres.clientSideConfig ConfigError Configured BadResponse (Just "ws://localhost:8080/pgproxy") (Just "{\"sessionId\": \"1f137f5f-43ec-4393-b5e8-bf195015e697\"}")
```

Also sent in the request is `requestId`, which is an `Int`. This id is echoed back in the response, which allows correlation between client and server logs.

### Responses

Responses are JSON and of the format for successful responses:

```js
{
	"requestId": "13",
	"success": true,
	// the rest of the keys for the Service's response
}
```

And for non-successful responses:

```js
{
	"requestId": "13",
	"success": false,
	"error": "Error message"
}
```

## Test code

The App in the test code is an example server that houses PGProxy which is a service. The App is written to support multiple services with the same interface as PGProxy. This was not necessary for this single service, but was more as a proof of concept for additional services which is how Panoramic will be using this module.

If you write your own server, this should be a good starting point.
