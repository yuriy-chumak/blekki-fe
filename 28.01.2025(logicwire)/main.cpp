#include <memory>
#include <iostream>
#include <functional>
#include <vector>

typedef unsigned int uint;

class Board
{
private:
    int width = 0;
    int height = 0;
    uint* board = 0;

public:
    class Row {
    private:
        uint* ptr;
    public:
        Row(uint* ptr) : ptr(ptr) {}
        uint operator[](const int i) {
            return ptr[i];
        }
        void wr(const int i, uint q) {
            ptr[i] = q;
        }
    };

public:
    Board(int width, int height)
    :   Board(nullptr, width, height)
    {}
    ~Board() {
        if (board) delete[] board;
    }

    Board(const char* circuit, const int width, const int height)
    {
        this->width = width;
        this->height = height;
        
        this->board = new uint[width * height];
        if (circuit) {
            for (int i = 0; i < width * height; i++)
                board[i] = (circuit[i] != ' ') ? circuit[i] : 0;
        }
    }

    int getWidth() { return width; }
    int getHeight() { return height; }

    Row operator[](const int row) {
        return Row(&board[row * width]);
    }

    uint operator()(const int x, const int y)
    {
        return (*this)[y][x];
    }

    void print()
    {
        for (int row = 0; row < height; row++) {
            for (int column = 0; column < width; column++) {
                std::cout << (char)((*this)[row][column]);
            }
            std::cout << std::endl;
        }
    }
};


class Gate
{
public:
    const int id;
    const int source;
    const int drain;

public:
    Gate(int id, int source, int drain)
    :   id(id), source(source), drain(drain)
    {}

    void print() {
        std::cout << "gate " << id << ": " << source << " -> " << drain << std::endl;
    }
};

class LogicWire
{
private:
    Board board;
    std::vector<bool> wires;
    std::vector<Gate> gates;
    // DEBUG:
    Board map;

public:
    LogicWire(const char* circuit, const int width, const int height)
    :   board(circuit, width, height),
        map(width, height)
    {
        // map(width, height); // do a local if no debug needed
        // select all wires
        int wire = 0;
        for (int x = 1; x < width-1; x++) {
            for (int y = 1; y < height-1; y++) {
                uint b = board(x,y);
                uint m = map(x,y);
                if (board(x,y) && !map(x,y)) {
                    ++wire;
                    // find all connected
                    std::function<void(int, int)> walker;
                    walker = [&](int x, int y) {
                        if (map(x,y))
                            return;
                        map[y].wr(x, wire);
                        if (board(x+1, y))
                            walker(x+1, y);
                        if (board(x-1, y))
                            walker(x-1, y);
                        if (board(x, y+1))
                            walker(x, y+1);
                        if (board(x, y-1))
                            walker(x, y-1);
                    };
                    walker(x, y);
                }
            }
        }
        // by default all wires are unpowered
        wires.resize(wire+1); // 1 for non existent "0" wire

        std::cout << "wires: " << wire << std::endl;
        // DEBUG: print all wires on map
        for (int row = 0; row < height; row++) {
            for (int column = 0; column < width; column++) {
                if (map[row][column])
                    std::cout << map[row][column];
                else
                    std::cout << ' ';
            }
            std::cout << std::endl;
        }

        // 2. find all gates
        int gate = 0;
        for (int y = 2; y < height-2; y++) {
            for (int x = 2; x < width-2; x++) {
                long mask = 0x000000000;
                if (board(x-1, y-1)) mask |= 0x100000000;
                if (board(x  , y-1)) mask |= 0x010000000;
                if (board(x+1, y-1)) mask |= 0x001000000;
                if (board(x-1, y  )) mask |= 0x000100000;
                if (board(x  , y  )) mask |= 0x000010000;
                if (board(x+1, y  )) mask |= 0x000001000;
                if (board(x-1, y+1)) mask |= 0x000000100;
                if (board(x  , y+1)) mask |= 0x000000010;
                if (board(x+1, y+1)) mask |= 0x000000001;
                // four rotations of gates:
                switch (mask) {
                // xx   left-to-right
                // x x
                // xx
                case 0x110101110:
                    ++gate;
                    gates.push_back(Gate(gate, map(x-1,y), map(x+1,y)));
                    break;

                //  xx  right-to-left
                // x x
                //  xx
                case 0x011101011:
                    ++gate;
                    gates.push_back(Gate(gate, map(x+1,y), map(x-1,y)));
                    break;

                // xxx  top-to-bottom
                // x x
                //  x
                case 0x111101010:
                    ++gate;
                    gates.push_back(Gate(gate, map(x,y-1), map(x,y+1)));
                    break;

                //  x   bottom-to-top
                // x x
                // xxx
                case 0x010101111:
                    ++gate;
                    gates.push_back(Gate(gate, map(x,y+1), map(x,y-1)));
                    break;

                default:
                    // not a gate, do nothing
                    break;
                }
            }
        }
        std::cout << "gates: " << gate << std::endl;

        for (int i = 0; i < gates.size(); i++)
            gates[i].print();
    }

    void Simulate() {
        // new unpowered wires array
        std::vector<bool> new_states;
        new_states.resize(wires.size());
        for (int i = 0; i < gates.size(); i++) {
            Gate& gate = gates[i];
            bool source_powered = wires[gate.source];
            if (!source_powered)
                new_states[gate.drain] = true;
        }
        wires = new_states;
    }

    void PowerTheWire(int id) {
        wires[id] = true;
    }

    void print() {
        const char* bright = "\033[31m";
        const char* normal = "\033[37m";
        const char* end = "\033[0m";

        const int width = board.getWidth();
        const int height = board.getHeight();
        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                int wire = map(x, y);
                char v = (char)(board[y][x]);
                std::cout << (wires[wire] ? bright : normal)
                          << (v ? v : ' ');
            }
            std::cout << std::endl;
        }
    }
};


// --------------------------------------------------------------------------
char* circuit = (char*) // 18x12
// 012345678901234567
  "                  "//0
  "  xx              "//1
  " xx oooooooooo    "//2
  "  xx o o   o o    "//3
  "      +     +     "//4
  "     ++ ++ ++     "//5
  "   +++ ++ ++ ++   "//6
  "   + ++ ++ ++ +   "//7
  "   +          +   "//8
  "   ++++++++++++   "//9
  "          +       "//10
  "                  ";//11

int main(int argc, char** argv)
{
    LogicWire lw(circuit, 18, 12);

    lw.print();
    std::cout << std::endl;
    lw.PowerTheWire(1);
    lw.print();
    std::cout << std::endl;

    for (int i = 0; i < 15; i++) {
        std::cout << "----------------" << std::endl;
        std::cout << "Simulation " << i << std::endl;
        lw.Simulate();
        lw.print();
        std::cout << std::endl;
    }
}
