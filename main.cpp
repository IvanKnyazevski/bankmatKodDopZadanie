#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>
#include <map>
#include <variant>
#include <climits>
#include <functional>
#include <cctype>

using namespace std;

// -------------------- lightweight JSON parser --------------------
struct JSONValue;

using JSONArray = vector<JSONValue>;
using JSONObject = map<string, JSONValue>;
using JSONBase = variant<string, int, double, JSONArray, JSONObject>;

struct JSONValue : JSONBase {
    using JSONBase::JSONBase;
};

class JSONParser {
    string src;
    size_t pos;

    void skip_ws() {
        while (pos < src.size() && isspace(static_cast<unsigned char>(src[pos])))
            ++pos;
    }

    char peek() {
        skip_ws();
        return pos < src.size() ? src[pos] : '\0';
    }

    char get() {
        skip_ws();
        return pos < src.size() ? src[pos++] : '\0';
    }

    string parse_string() {
        char quote = get(); // "
        string s;
        while (pos < src.size() && src[pos] != quote) {
            if (src[pos] == '\\') {
                ++pos;
                if (pos < src.size()) {
                    switch (src[pos]) {
                    case '"': s += '"'; break;
                    case '\\': s += '\\'; break;
                    case '/': s += '/'; break;
                    case 'n': s += '\n'; break;
                    case 't': s += '\t'; break;
                    default: s += src[pos]; break;
                    }
                }
            }
            else {
                s += src[pos];
            }
            ++pos;
        }
        if (pos < src.size()) ++pos; // skip closing quote
        return s;
    }

    int parse_int() {
        skip_ws();
        size_t start = pos;
        while (pos < src.size() && isdigit(static_cast<unsigned char>(src[pos])))
            ++pos;
        return stoi(src.substr(start, pos - start));
    }

    JSONValue parse_object() {
        get(); // '{'
        JSONObject obj;
        if (peek() == '}') { get(); return obj; }
        while (true) {
            skip_ws();
            string key = parse_string();
            skip_ws();
            get(); // ':'
            obj[key] = parse_value();
            skip_ws();
            char c = get();
            if (c == '}') break;
            // else c must be ','
        }
        return obj;
    }

    JSONValue parse_array() {
        get(); // '['
        JSONArray arr;
        if (peek() == ']') { get(); return arr; }
        while (true) {
            arr.push_back(parse_value());
            skip_ws();
            char c = get();
            if (c == ']') break;
        }
        return arr;
    }

public:
    JSONParser(const string& s) : src(s), pos(0) {}

    JSONValue parse_value() {
        skip_ws();
        if (pos >= src.size()) return 0;
        char c = peek();
        if (c == '"') {
            return parse_string();
        }
        else if (c == '{') {
            return parse_object();
        }
        else if (c == '[') {
            return parse_array();
        }
        else {
            return parse_int();
        }
    }
};

// -------------------- problem solving --------------------
// Returns reachable bitset for amounts 0..max_amount
vector<bool> getReachable(const vector<pair<int, int>>& denoms, int max_amount) {
    vector<bool> dp(max_amount + 1, false);
    dp[0] = true;
    for (auto& p : denoms) {
        int d = p.first;
        int cnt = p.second;
        if (cnt == 0) continue;
        int k = 1;
        while (cnt > 0) {
            int take = min(k, cnt);
            cnt -= take;
            int val = take * d;
            for (int a = max_amount; a >= val; --a) {
                if (dp[a - val]) dp[a] = true;
            }
            k <<= 1;
        }
    }
    return dp;
}

bool canMake(const vector<pair<int, int>>& denoms, int amount) {
    if (amount < 0) return false;
    auto dp = getReachable(denoms, amount);
    return dp[amount];
}

vector<pair<int, int>> solveMAX(vector<pair<int, int>> wallet, int amount) {
    // sort descending by denomination
    sort(wallet.begin(), wallet.end(),
        [](const pair<int, int>& a, const pair<int, int>& b) { return a.first > b.first; });
    vector<pair<int, int>> result;
    function<bool(int, int)> dfs = [&](int idx, int rem) -> bool {
        if (idx == (int)wallet.size()) return rem == 0;
        int d = wallet[idx].first;
        int cnt = wallet[idx].second;
        vector<pair<int, int>> suffix(wallet.begin() + idx + 1, wallet.end());
        auto reachable = getReachable(suffix, rem);
        int maxk = min(cnt, rem / d);
        for (int k = maxk; k >= 0; --k) {
            int newrem = rem - k * d;
            if (reachable[newrem]) {
                if (k > 0) result.push_back({ d, k });
                if (dfs(idx + 1, newrem)) return true;
                if (k > 0) result.pop_back();
            }
        }
        return false;
        };
    if (dfs(0, amount)) return result;
    return {};
}

vector<pair<int, int>> solveMIN(vector<pair<int, int>> wallet, int amount) {
    // sort ascending by denomination
    sort(wallet.begin(), wallet.end(),
        [](const pair<int, int>& a, const pair<int, int>& b) { return a.first < b.first; });
    vector<pair<int, int>> result;
    function<bool(int, int)> dfs = [&](int idx, int rem) -> bool {
        if (idx == (int)wallet.size()) return rem == 0;
        int d = wallet[idx].first;
        int cnt = wallet[idx].second;
        vector<pair<int, int>> suffix(wallet.begin() + idx + 1, wallet.end());
        auto reachable = getReachable(suffix, rem);
        int maxk = min(cnt, rem / d);
        for (int k = maxk; k >= 0; --k) {
            int newrem = rem - k * d;
            if (reachable[newrem]) {
                if (k > 0) result.push_back({ d, k });
                if (dfs(idx + 1, newrem)) return true;
                if (k > 0) result.pop_back();
            }
        }
        return false;
        };
    if (dfs(0, amount)) return result;
    return {};
}

vector<pair<int, int>> solveUNIFORM(const vector<pair<int, int>>& wallet, int amount) {
    int n = wallet.size();
    if (n == 0) return {};
    int min_avail = INT_MAX, max_avail = 0;
    long long sum_d = 0;
    for (auto& p : wallet) {
        min_avail = min(min_avail, p.second);
        max_avail = max(max_avail, p.second);
        sum_d += p.first;
    }
    if (min_avail == INT_MAX) min_avail = 0;
    int best_diff = INT_MAX;
    int best_L = -1, best_R = -1;
    vector<int> denoms(n), avails(n);
    for (int i = 0; i < n; ++i) {
        denoms[i] = wallet[i].first;
        avails[i] = wallet[i].second;
    }

    for (int L = 0; L <= min_avail; ++L) {
        long long base = L * sum_d;
        if (base > amount) continue;
        long long rem_amt = amount - base;
        int low = L, high = max_avail, R_found = -1;
        while (low <= high) {
            int mid = (low + high) / 2;
            bool possible = true;
            vector<pair<int, int>> items;
            for (int i = 0; i < n; ++i) {
                int upper = min(avails[i], mid);
                if (upper < L) { possible = false; break; }
                int max_y = upper - L;
                if (max_y > 0) items.push_back({ denoms[i], max_y });
            }
            if (!possible) { low = mid + 1; continue; }
            if (canMake(items, rem_amt)) {
                R_found = mid;
                high = mid - 1;
            }
            else {
                low = mid + 1;
            }
        }
        if (R_found != -1) {
            int diff = R_found - L;
            if (diff < best_diff) {
                best_diff = diff;
                best_L = L;
                best_R = R_found;
                if (diff == 0) break;
            }
        }
    }

    if (best_L == -1) return {};

    // Reconstruct one solution with L = best_L, R = best_R
    vector<pair<int, int>> rec_items;
    for (int i = 0; i < n; ++i) {
        int upper = min(avails[i], best_R);
        int max_y = upper - best_L;
        if (max_y > 0) rec_items.push_back({ denoms[i], max_y });
    }
    // Sort descending to quickly find a valid assignment (any is OK)
    sort(rec_items.begin(), rec_items.end(),
        [](const pair<int, int>& a, const pair<int, int>& b) { return a.first > b.first; });

    long long rem_amt = amount - best_L * sum_d;
    vector<pair<int, int>> y_result; // (denom, y)
    function<bool(int, int, const vector<pair<int, int>>&)> dfs_y =
        [&](int idx, int rem, const vector<pair<int, int>>& items) -> bool {
        if (idx == (int)items.size()) return rem == 0;
        int d = items[idx].first, cnt = items[idx].second;
        vector<pair<int, int>> suffix(items.begin() + idx + 1, items.end());
        auto reachable = getReachable(suffix, rem);
        int maxk = min(cnt, rem / d);
        for (int k = maxk; k >= 0; --k) {
            if (reachable[rem - k * d]) {
                if (k > 0) y_result.push_back({ d, k });
                if (dfs_y(idx + 1, rem - k * d, items)) return true;
                if (k > 0) y_result.pop_back();
            }
        }
        return false;
        };

    if (!dfs_y(0, rem_amt, rec_items)) return {};

    // Combine L and y_result into final counts
    vector<int> counts(n, best_L);
    for (auto& p : y_result) {
        for (int i = 0; i < n; ++i) {
            if (wallet[i].first == p.first) {
                counts[i] += p.second;
                break;
            }
        }
    }

    vector<pair<int, int>> dispense;
    for (int i = 0; i < n; ++i) {
        if (counts[i] > 0) dispense.push_back({ wallet[i].first, counts[i] });
    }
    return dispense;
}

// -------------------- JSON output helper --------------------
string toJSON(const vector<pair<int, int>>& dispense) {
    string s = "[";
    for (size_t i = 0; i < dispense.size(); ++i) {
        if (i) s += ",";
        s += "[" + to_string(dispense[i].first) + "," + to_string(dispense[i].second) + "]";
    }
    s += "]";
    return s;
}

// -------------------- main --------------------
int main() {
    // read input file
    ifstream fin("input.json");
    if (!fin) {
        cerr << "Cannot open input.json" << endl;
        return 1;
    }
    string raw((istreambuf_iterator<char>(fin)), istreambuf_iterator<char>());
    fin.close();

    JSONParser parser(raw);
    JSONValue root = parser.parse_value(); // must be an array

    if (!holds_alternative<JSONArray>(root)) {
        cerr << "Root is not an array" << endl;
        return 1;
    }
    JSONArray& arr = get<JSONArray>(root);
    if (arr.empty()) return 1;
    if (!holds_alternative<JSONObject>(arr[0])) return 1;
    JSONObject& obj = get<JSONObject>(arr[0]);

    // extract wallet
    auto wallet_val = obj.at("wallet");
    vector<pair<int, int>> wallet;
    if (holds_alternative<JSONArray>(wallet_val)) {
        JSONArray& w = get<JSONArray>(wallet_val);
        for (auto& item : w) {
            if (holds_alternative<JSONArray>(item)) {
                JSONArray& pair_arr = get<JSONArray>(item);
                if (pair_arr.size() == 2) {
                    int denom = get<int>(pair_arr[0]);
                    int cnt = get<int>(pair_arr[1]);
                    wallet.push_back({ denom, cnt });
                }
            }
        }
    }

    int amount = get<int>(obj.at("amount"));
    string strategy = get<string>(obj.at("strategy"));

    vector<pair<int, int>> dispense;
    if (strategy == "MAX") {
        dispense = solveMAX(wallet, amount);
    }
    else if (strategy == "MIN") {
        dispense = solveMIN(wallet, amount);
    }
    else if (strategy == "UNIFORM") {
        dispense = solveUNIFORM(wallet, amount);
    }

    // write output file
    ofstream fout("output.json");
    fout << "[{\"dispense\":" << toJSON(dispense) << "}]";
    fout.close();

    return 0;
}