
#ifndef _Serial_h_
#define _Serial_h_

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Opaque reference to a connection object.
 */
struct OpaqueSerialConnection;
typedef struct OpaqueSerialConnection SerialConnection;

typedef enum { UnspecifiedMode, TextMode, ByteMode } SerialMode;

typedef unsigned char ubyte;		/* unsigned byte data		*/

typedef void (*SerialDataHandlerP) (SerialConnection *, const ubyte *,
				    int len);

void SerialUsage ();
void SerialParseOptions (SerialConnection *, int *argc, char *argv[]);
SerialConnection *SerialInitialize ();
void SerialConnect (SerialConnection *, SerialDataHandlerP handler);
void SerialDisconnect (SerialConnection *);
void SerialDie (const char *msg, int code);
SerialMode SerialGetMode (SerialConnection *);
void SerialSetMode (SerialConnection *sc, SerialMode mode);
void SerialSetHangupOnClose (SerialConnection *, int enable);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* _Serial_h_ */
