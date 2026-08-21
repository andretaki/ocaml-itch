// A C++ baseline for the OCaml ITCH parser.
//
// The point of this program is not to be a good ITCH library -- it is to do
// exactly the same work as lib/itch/checksum.ml, so that a timing comparison
// means something. It mmaps the file, walks the 2-byte length framing, decodes
// the same nine message types, and folds every decoded field into the same
// accumulators using the same arithmetic.
//
// Both programs print the aggregate. If the two lines are not identical, the
// comparison is void: either they decoded different things, or a compiler
// deleted work that was never observed. Matching output is the evidence that
// the timings are comparable.
//
//   g++ -O3 -march=native -std=c++20 -o baseline baseline.cpp

#include <cstdint>
#include <cstdio>
#include <cstring>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>
#include <algorithm>
#include <chrono>
#include <cstdlib>

namespace {

inline uint16_t be16(const uint8_t *p) {
  uint16_t v;
  std::memcpy(&v, p, 2);
  return __builtin_bswap16(v);
}
inline uint32_t be32(const uint8_t *p) {
  uint32_t v;
  std::memcpy(&v, p, 4);
  return __builtin_bswap32(v);
}
inline uint64_t be64(const uint8_t *p) {
  uint64_t v;
  std::memcpy(&v, p, 8);
  return __builtin_bswap64(v);
}
// 48-bit timestamp, assembled exactly as Wire.timestamp does.
inline int64_t be48(const uint8_t *p) {
  return (static_cast<int64_t>(be16(p)) << 32) | static_cast<int64_t>(be32(p + 2));
}

struct Checksum {
  int64_t messages = 0, adds = 0, executes = 0, cancels = 0, deletes = 0;
  int64_t replaces = 0, directories = 0, system_events = 0, others = 0;
  int64_t sum_shares = 0, sum_prices = 0;
  // Three separate order-reference accumulators, mirroring checksum.ml. A
  // single xor of order_ref and match_number is unchanged when those two are
  // swapped, which is the one mistake this comparison most needs to catch.
  int64_t xor_order_refs = 0, xor_match_numbers = 0, xor_new_refs = 0;
  int64_t xor_timestamps = 0, max_locate = 0;

  inline void note(int64_t stock_locate, int64_t timestamp) {
    messages += 1;
    xor_timestamps ^= timestamp;
    if (stock_locate > max_locate) max_locate = stock_locate;
  }

  void print() const {
    std::printf(
        "messages=%ld adds=%ld executes=%ld cancels=%ld deletes=%ld replaces=%ld "
        "directories=%ld system_events=%ld others=%ld sum_shares=%ld sum_prices=%ld "
        "xor_order_refs=%ld xor_match_numbers=%ld xor_new_refs=%ld "
        "xor_timestamps=%ld max_locate=%ld\n",
        messages, adds, executes, cancels, deletes, replaces, directories,
        system_events, others, sum_shares, sum_prices, xor_order_refs,
        xor_match_numbers, xor_new_refs, xor_timestamps, max_locate);
  }
};

size_t consume(Checksum &c, const uint8_t *buf, size_t len) {
  size_t p = 0;
  while (p + 2 <= len) {
    const uint32_t message_length = be16(buf + p);
    if (message_length == 0 || p + 2 + message_length > len) break;
    const uint8_t *m = buf + p + 2;
    const char type = static_cast<char>(m[0]);
    const int64_t stock_locate = be16(m + 1);
    const int64_t timestamp = be48(m + 5);

    switch (type) {
      case 'A':
      case 'F': {
        const bool attributed = (type == 'F');
        c.note(stock_locate, timestamp);
        c.adds += 1;
        c.sum_shares += static_cast<int64_t>(be32(m + 20)) +
                        static_cast<int64_t>(static_cast<uint8_t>(m[19])) +
                        (attributed ? 1 : 0);
        c.sum_prices += static_cast<int64_t>(be32(m + 32));
        c.xor_order_refs ^= static_cast<int64_t>(be64(m + 11));
        break;
      }
      case 'E': {
        c.note(stock_locate, timestamp);
        c.executes += 1;
        c.sum_shares += static_cast<int64_t>(be32(m + 19));
        c.xor_order_refs ^= static_cast<int64_t>(be64(m + 11));
        c.xor_match_numbers ^= static_cast<int64_t>(be64(m + 23));
        break;
      }
      case 'C': {
        c.note(stock_locate, timestamp);
        c.executes += 1;
        c.sum_shares +=
            static_cast<int64_t>(be32(m + 19)) + (m[31] == 'Y' ? 1 : 0);
        c.sum_prices += static_cast<int64_t>(be32(m + 32));
        c.xor_order_refs ^= static_cast<int64_t>(be64(m + 11));
        c.xor_match_numbers ^= static_cast<int64_t>(be64(m + 23));
        break;
      }
      case 'X': {
        c.note(stock_locate, timestamp);
        c.cancels += 1;
        c.sum_shares += static_cast<int64_t>(be32(m + 19));
        c.xor_order_refs ^= static_cast<int64_t>(be64(m + 11));
        break;
      }
      case 'D': {
        c.note(stock_locate, timestamp);
        c.deletes += 1;
        c.xor_order_refs ^= static_cast<int64_t>(be64(m + 11));
        break;
      }
      case 'U': {
        c.note(stock_locate, timestamp);
        c.replaces += 1;
        c.sum_shares += static_cast<int64_t>(be32(m + 27));
        c.sum_prices += static_cast<int64_t>(be32(m + 31));
        c.xor_order_refs ^= static_cast<int64_t>(be64(m + 11));
        c.xor_new_refs ^= static_cast<int64_t>(be64(m + 19));
        break;
      }
      case 'S': {
        c.note(stock_locate, timestamp);
        c.system_events += 1;
        c.sum_shares += static_cast<int64_t>(be16(m + 3)) +
                        static_cast<int64_t>(static_cast<uint8_t>(m[11]));
        break;
      }
      case 'R': {
        c.note(stock_locate, timestamp);
        c.directories += 1;
        c.sum_shares += static_cast<int64_t>(be32(m + 21));
        break;
      }
      default: {
        // Matches Checksum.on_other, which counts the message but does not
        // touch the timestamp or locate accumulators.
        c.messages += 1;
        c.others += 1;
        c.sum_shares += static_cast<int64_t>(static_cast<uint8_t>(type)) +
                        static_cast<int64_t>(message_length);
        break;
      }
    }
    p += 2 + message_length;
  }
  return p;
}

}  // namespace

int main(int argc, char **argv) {
  if (argc < 2) {
    std::fprintf(stderr, "usage: %s FILE [repeats]\n", argv[0]);
    return 2;
  }
  const int repeats = (argc > 2) ? std::atoi(argv[2]) : 1;

  const int fd = ::open(argv[1], O_RDONLY);
  if (fd < 0) { std::perror("open"); return 1; }
  struct stat st{};
  if (::fstat(fd, &st) != 0) { std::perror("fstat"); return 1; }
  const size_t len = static_cast<size_t>(st.st_size);
  auto *buf = static_cast<const uint8_t *>(
      ::mmap(nullptr, len, PROT_READ, MAP_PRIVATE, fd, 0));
  if (buf == MAP_FAILED) { std::perror("mmap"); return 1; }

  { Checksum warm; consume(warm, buf, len); }  // warm the page cache

  double best = 1e18;
  size_t consumed = 0;
  Checksum result;
  for (int i = 0; i < repeats; ++i) {
    Checksum c;
    const auto start = std::chrono::steady_clock::now();
    consumed = consume(c, buf, len);
    const auto stop = std::chrono::steady_clock::now();
    const double elapsed =
        std::chrono::duration<double>(stop - start).count();
    if (elapsed < best) best = elapsed;
    result = c;
  }

  result.print();
  std::printf("consumed        %zu bytes\n", consumed);
  std::printf("fastest of %d    %.4f s  (%.2f M msg/s, %.1f MB/s)\n", repeats,
              best, static_cast<double>(result.messages) / best / 1e6,
              static_cast<double>(consumed) / best / 1e6);
  ::munmap(const_cast<uint8_t *>(buf), len);
  ::close(fd);
  return 0;
}
