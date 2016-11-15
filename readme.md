# Postgres Authenticating Proxy Server in Elm

> This is a Proxy Server for the Elm [Postgres Effects Manager](https://github.com/panosoft/elm-postgres).

> This proxy server allows the Postgres Effects Manager to be used on the **client**.

> The Postgres Effects Manager's native code when run on the client will delegate to a proxy server. This Proxy Server can be used directly or as a reference implementation.

> This implementation is an authenticating proxy server. It delegates authentication for each request to the application via an injected authentication function. Once authenticated, it honors the request.

> It's built with the same Postgres Effects Manager as the client except it uses the server native code to make calls to the DB on behalf of the client.

> Communication between client and server uses Websockets using the client library [elm-websocket-browser](https://github.com/panosoft/elm-websocket-browser) and the server library [elm-websocket-server](https://github.com/panosoft/elm-websocket-server)

## Details


### Requests

Requests are JSON and of the format:

```js
{
	"sessionId": "1f137f5f-43ec-4393-b5e8-bf195015e697",
	"requestId": "13",
	// the rest of the keys for the Service's request
}
```
The only expectation for `sessionId` is that it's a String.

`requestId` is just echoed back in the response. It's an Int.

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

The App in the test code is an example server that houses PGProxy which is a service. It's written to support multiple services with the same interface as PGProxy. This was not necessary for this single service, but was more as a proof of concept for additional services which is how Panoramic will be using this module.

When you write your own server, then this should be a good starting point.
