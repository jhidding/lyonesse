#include <random>
#include <cstdint>
#include <chrono>

std::mt19937 random_engine;

extern "C" void random_seed(uint64_t seed)
{
    random_engine.seed(seed);
}

extern "C" void randomize_timer()
{
    uint64_t n = std::chrono::system_clock::now()
        .time_since_epoch().count();
    random_seed(n);
}

extern "C" int random_integer(int a, int b)
{
    return std::uniform_int_distribution<int>(a, b)(random_engine);
}

