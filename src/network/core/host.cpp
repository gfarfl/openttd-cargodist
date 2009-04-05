/* $Id$ */

/** @file host.cpp Functions related to getting host specific data (IPs). */

#ifdef ENABLE_NETWORK

#include "../../stdafx.h"
#include "../../debug.h"
#include "os_abstraction.h"
#include "address.h"
#include "../../core/alloc_func.hpp"
#include "../../string_func.h"

/**
 * Internal implementation for finding the broadcast IPs.
 * This function is implemented multiple times for multiple targets.
 * @param broadcast the list of broadcasts to write into.
 * @param limit     the maximum number of items to add.
 */
static int NetworkFindBroadcastIPsInternal(NetworkAddress *broadcast, int limit);

#if defined(PSP)
static int NetworkFindBroadcastIPsInternal(NetworkAddress *broadcast, int limit) // PSP implementation
{
	return 0;
}

#elif defined(BEOS_NET_SERVER) /* doesn't have neither getifaddrs or net/if.h */
/* Based on Andrew Bachmann's netstat+.c. Big thanks to him! */
int _netstat(int fd, char **output, int verbose);

int seek_past_header(char **pos, const char *header)
{
	char *new_pos = strstr(*pos, header);
	if (new_pos == 0) {
		return B_ERROR;
	}
	*pos += strlen(header) + new_pos - *pos + 1;
	return B_OK;
}

static int NetworkFindBroadcastIPsInternal(NetworkAddress *broadcast, int limit) // BEOS implementation
{
	int sock = socket(AF_INET, SOCK_DGRAM, 0);

	if (sock < 0) {
		DEBUG(net, 0, "[core] error creating socket");
		return 0;
	}

	char *output_pointer = NULL;
	int output_length = _netstat(sock, &output_pointer, 1);
	if (output_length < 0) {
		DEBUG(net, 0, "[core] error running _netstat");
		return 0;
	}

	int index;
	char **output = &output_pointer;
	if (seek_past_header(output, "IP Interfaces:") == B_OK) {
		while (index != limit) {

			uint32 n, fields, read;
			uint8 i1, i2, i3, i4, j1, j2, j3, j4;
			struct in_addr inaddr;
			uint32 ip;
			uint32 netmask;

			fields = sscanf(*output, "%u: %hhu.%hhu.%hhu.%hhu, netmask %hhu.%hhu.%hhu.%hhu%n",
												&n, &i1, &i2, &i3, &i4, &j1, &j2, &j3, &j4, &read);
			read += 1;
			if (fields != 9) {
				break;
			}

			ip      = (uint32)i1 << 24 | (uint32)i2 << 16 | (uint32)i3 << 8 | (uint32)i4;
			netmask = (uint32)j1 << 24 | (uint32)j2 << 16 | (uint32)j3 << 8 | (uint32)j4;

			if (ip != INADDR_LOOPBACK && ip != INADDR_ANY) {
				sockaddr_storage address;
				memset(&address, 0, sizeof(address));
				((sockaddr_in*)&address)->sin_addr.s_addr = htonl(ip | ~netmask);
				broadcast[index] = NetworkAddress(address, sizeof(sockaddr));
				index++;
			}
			if (read < 0) {
				break;
			}
			*output += read;
		}
		closesocket(sock);
	}

	return index;
}

#elif defined(HAVE_GETIFADDRS)
static int NetworkFindBroadcastIPsInternal(NetworkAddress *broadcast, int limit) // GETIFADDRS implementation
{
	struct ifaddrs *ifap, *ifa;

	if (getifaddrs(&ifap) != 0) return 0;

	int index = 0;
	for (ifa = ifap; ifa != NULL && index != limit; ifa = ifa->ifa_next) {
		if (!(ifa->ifa_flags & IFF_BROADCAST)) continue;
		if (ifa->ifa_broadaddr == NULL) continue;
		if (ifa->ifa_broadaddr->sa_family != AF_INET) continue;

		broadcast[index] = NetworkAddress(ifa->ifa_broadaddr, sizeof(sockaddr));
		index++;
	}
	freeifaddrs(ifap);

	return index;
}

#elif defined(WIN32)
static int NetworkFindBroadcastIPsInternal(NetworkAddress *broadcast, int limit) // Win32 implementation
{
	SOCKET sock = socket(AF_INET, SOCK_DGRAM, 0);
	if (sock == INVALID_SOCKET) return 0;

	DWORD len = 0;
	INTERFACE_INFO *ifo = AllocaM(INTERFACE_INFO, limit);
	memset(ifo, 0, limit * sizeof(*ifo));
	if ((WSAIoctl(sock, SIO_GET_INTERFACE_LIST, NULL, 0, &ifo[0], limit * sizeof(*ifo), &len, NULL, NULL)) != 0) {
		closesocket(sock);
		return 0;
	}

	int index = 0;
	for (uint j = 0; j < len / sizeof(*ifo) && index != limit; j++) {
		if (ifo[j].iiFlags & IFF_LOOPBACK) continue;
		if (!(ifo[j].iiFlags & IFF_BROADCAST)) continue;

		sockaddr_storage address;
		memset(&address, 0, sizeof(address));
		/* iiBroadcast is unusable, because it always seems to be set to 255.255.255.255. */
		memcpy(&address, &ifo[j].iiAddress.Address, sizeof(sockaddr));
		((sockaddr_in*)&address)->sin_addr.s_addr = ifo[j].iiAddress.AddressIn.sin_addr.s_addr | ~ifo[j].iiNetmask.AddressIn.sin_addr.s_addr;
		broadcast[index] = NetworkAddress(address, sizeof(sockaddr));
		index++;
	}

	closesocket(sock);
	return index;
}

#else /* not HAVE_GETIFADDRS */

#include "../../string_func.h"

static int NetworkFindBroadcastIPsInternal(NetworkAddress *broadcast, int limit) // !GETIFADDRS implementation
{
	SOCKET sock = socket(AF_INET, SOCK_DGRAM, 0);
	if (sock == INVALID_SOCKET) return 0;

	char buf[4 * 1024]; // Arbitrary buffer size
	struct ifconf ifconf;

	ifconf.ifc_len = sizeof(buf);
	ifconf.ifc_buf = buf;
	if (ioctl(sock, SIOCGIFCONF, &ifconf) == -1) {
		closesocket(sock);
		return 0;
	}

	const char *buf_end = buf + ifconf.ifc_len;
	int index = 0;
	for (const char *p = buf; p < buf_end && index != limit;) {
		const struct ifreq *req = (const struct ifreq*)p;

		if (req->ifr_addr.sa_family == AF_INET) {
			struct ifreq r;

			strecpy(r.ifr_name, req->ifr_name, lastof(r.ifr_name));
			if (ioctl(sock, SIOCGIFFLAGS, &r) != -1 &&
					r.ifr_flags & IFF_BROADCAST &&
					ioctl(sock, SIOCGIFBRDADDR, &r) != -1) {
				broadcast[index++] = NetworkAddress(&r.ifr_broadaddr, sizeof(sockaddr));
			}
		}

		p += sizeof(struct ifreq);
#if defined(AF_LINK) && !defined(SUNOS)
		p += req->ifr_addr.sa_len - sizeof(struct sockaddr);
#endif
	}

	closesocket(sock);

	return index;
}
#endif /* all NetworkFindBroadcastIPsInternals */

/**
 * Find the IPv4 broadcast addresses; IPv6 uses a completely different
 * strategy for broadcasting.
 * @param broadcast the list of broadcasts to write into.
 * @param limit     the maximum number of items to add.
 */
void NetworkFindBroadcastIPs(NetworkAddress *broadcast, int limit)
{
	int count = NetworkFindBroadcastIPsInternal(broadcast, limit);

	/* Make sure the list is terminated. */
	broadcast[count] = NetworkAddress("");

	/* Now display to the debug all the detected ips */
	DEBUG(net, 3, "Detected broadcast addresses:");
	for (int i = 0; !StrEmpty(broadcast[i].GetHostname()); i++) {
		DEBUG(net, 3, "%d) %s", i, broadcast[i].GetHostname());
	}
}

#endif /* ENABLE_NETWORK */
