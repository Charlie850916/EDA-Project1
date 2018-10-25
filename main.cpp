#include <bits/stdc++.h>
#include "pdqsort.h"

using namespace std;

const int MAXN = 30;
const int MAXM = 300000;
const int THRESHOLD = 7;
const double TIMELIMIT = 6600.0;

fstream fin, fout;
int n, m;
vector<string> SOP, covered;
clock_t st;

struct pTerm
{
    string term;
    int weight;
    bool operator==(const pTerm &rhs) const
    {
        return term==rhs.term;
    }
    bool operator<(const pTerm &rhs) const
    {
        return term<rhs.term;
    }
    int literalCount() const
    {
        int cnt = 0;
        for(int i=0 ; i<term.length() ; ++i)
            if(term[i]!='-')
                cnt++;
        return cnt;
    }
};

set<pTerm> primeTerm;
vector<pTerm> Term[MAXN][MAXN], ans;

int ones(string term, char c='1')
{
    int cnt = 0;
    for(int i=0 ; i<term.length() ; ++i)
        if(term[i]==c)
            cnt++;
    return cnt;
}

void init()
{
    fin >> n >> m;
    for(int i=0 ; i<m ; ++i)
    {
        string in;
        fin >> in;
        SOP.push_back(in);
    }
    fin.close();
}

bool cmpWeight(pTerm a, pTerm b)
{
    return a.weight < b.weight;
}

vector<pTerm> SOPterm;
void Rearrange()
{
    int colSum1[n], colSum2[n];
    for(int i=0 ; i<n ; ++i)
    {
        int cnt1 = 0, cnt2 = 0;
        for(int j=0 ; j<m ; ++j)
        {
            if(SOP[j][i]=='0')
                cnt1++;
            else if(SOP[j][i]=='1')
                cnt2++;
            else
            {
                cnt1++;
                cnt2++;
            }
        }
        colSum1[i] = cnt1;
        colSum2[i] = cnt2;
    }
    for(int i=0 ; i<m ; ++i)
    {
        int w = 0;
        for(int j=0 ; j<n ; ++j)
        {
            if(SOP[i][j]=='0')
                w += colSum1[j];
            else if(SOP[i][j]=='1')
                w += colSum2[j];
            else
                w += colSum1[j]+colSum2[j];
        }
        pTerm p;
        p.term = SOP[i];
        p.weight = w;
        SOPterm.push_back(p);
    }
    pdqsort(SOPterm.begin(), SOPterm.end(), cmpWeight);
}

bool isOverTime()
{
    clock_t cur = clock();
    return TIMELIMIT < (double)(cur-st)/CLOCKS_PER_SEC;
}

vector<int> FreeSet[MAXM];
bool canCover(string term, int tag)
{
    for(auto id : FreeSet[tag])
    {
        if(SOPterm[tag].term[id]!='-')
            if(SOPterm[tag].term[id]!=term[id])
                return false;
    }
    return true;
}

bool flag[MAXM];
void Expand()
{
    for(int i=0 ; i<SOPterm.size() ; ++i)
    {
        flag[i] = true;
        for(int j=0 ; j<n ; ++j)
            if(SOPterm[i].term[j]!='-')
                FreeSet[i].push_back(j);
    }

    for(int i=0 ; i<SOPterm.size() ; ++i)
    {
        if(isOverTime())
            break;
        if(flag[i])
        {
            for(auto j : FreeSet[i])
            {
                if(SOPterm[i].term[j]!='-')
                {
                    bool done = false;
                    string tmp = SOPterm[i].term;
                    if(tmp[j]=='0')
                        tmp[j] = '1';
                    else
                        tmp[j] = '0';
                    for(int k=0 ; k<SOPterm.size() ; ++k)
                    {
                        if(i!=k && flag[k])
                        {
                            if(canCover(tmp, k))
                            {
                                done = true;
                                break;
                            }
                        }
                    }
                    if(done)
                        SOPterm[i].term[j] = '-';
                }
            }
            for(int k=0 ; k<SOP.size() ; ++k)
                if(i!=k && flag[k])
                    if(canCover(SOPterm[k].term, i))
                        flag[k] = false;
        }
    }
}

vector<string> allPoss;
void gen(int id, string s)
{
    if(id==n)
    {
        allPoss.push_back(s);
        return;
    }
    if(s[id]=='-')
    {
        string tmp = s;
        tmp[id] = '0';
        gen(id+1, tmp);
        tmp[id] = '1';
        gen(id+1, tmp);
    }
    else
        gen(id+1, s);
}

void Irredundant()
{
    for(int i=SOPterm.size()-1 ; i>=0 ; --i)
    {
        if(isOverTime())
            break;
        if(flag[i])
        {
            allPoss.clear();
            gen(0, SOPterm[i].term);
            int cnt = 0;
            for(string s : allPoss)
            {
                bool done = false;
                for(int j=0 ; j<SOPterm.size() ; ++j)
                {
                    if(i!=j && flag[j])
                    {
                        if(canCover(s,j))
                        {
                            done = true;
                            cnt++;
                            break;
                        }
                    }
                }
                if(!done)
                    break;
            }
            if(cnt==allPoss.size())
                flag[i] = false;
        }
    }

    for(int i=0 ; i<SOP.size() ; ++i)
        if(flag[i])
            ans.push_back(SOPterm[i]);
}

bool isCover(string p, string tag)
{
    for(int i=0 ; i<p.length() ; ++i)
        if(p[i]!='-')
            if(p[i]!=tag[i])
                return false;
    return true;
}

bool canCombine(string term1, string term2, string &res)
{
    int cnt = 0, id;
    for(int i=0 ; i<term1.length() ; ++i)
    {
        if(term1[i]!=term2[i])
        {
            if(term1[i]=='-'||term2[i]=='-')
                return false;
            if(++cnt>1)
                return false;
            id = i;
        }
    }
    res = term1;
    res[id] = '-';
    return true;
}

vector<string> Gray;
void bitString(int x,string prefix)
{
    if (x == 0)
        Gray.push_back(prefix);
    else
    {
        bitString(x-1,(prefix+"0"));
        bitString(x-1,(prefix+"1"));
    }
}

void QM_method()
{
    bitString(n,"");
    for(string s : Gray)
    {
        bool done = false;
        for(int i=0 ; i<SOP.size() ; ++i)
        {
            if(isCover(SOP[i], s))
            {
                done = true;
                pTerm p;
                p.term = s;
                covered.push_back(s);
                primeTerm.insert(p);
                Term[ones(s)][0].push_back(p);
                break;
            }
        }
        if(done)
            continue;
    }
    for(int i=1 ; i<=n ; ++i) // # -
        for(int j=0 ; j+1<=n ; ++j) // # 1
            for(pTerm term1 : Term[j][i-1])
                for(pTerm term2 : Term[j+1][i-1])
                {
                    pTerm res;
                    if(canCombine(term1.term, term2.term, res.term))
                    {
                        set<pTerm>::iterator it;
                        if((it=primeTerm.find(term1))!=primeTerm.end())
                            primeTerm.erase(it);
                        if((it=primeTerm.find(term2))!=primeTerm.end())
                            primeTerm.erase(it);
                        if(!primeTerm.count(res))
                        {
                            primeTerm.insert(res);
                            Term[j][i].push_back(res);
                        }
                    }
                }
}

vector<int> POS[MAXM];
bool valid[MAXM], essential[MAXM];
vector<pTerm> row;
int curAns = INT_MAX;

void dfs(int dep, int counter)
{
    if(counter >= curAns) // first cut
        return;

    for(int i=0 ; i<covered.size() ; ++i) // second cut
    {
        bool done = false;
        for(auto id : POS[i])
        {
            if(valid[id])
            {
                done = true;
                break;
            }
        }
        if(!done && POS[i].back() < dep)
            return;
    }

    if(dep==row.size())
    {
        ans.clear();
        curAns = counter;
        for(int i=0 ; i<row.size() ; ++i)
            if(valid[i])
                ans.push_back(row[i]);
        return;
    }

    if(!essential[dep])
        dfs(dep+1, counter);

    valid[dep] = true;
    dfs(dep+1, counter+row[dep].literalCount());
    valid[dep] = false;
}

void PetrickMethod()
{
    for(set<pTerm>::iterator it=primeTerm.begin() ; it!=primeTerm.end() ; ++it)
        row.push_back(*it);

    for(int i=0 ; i<row.size() ; ++i)
        valid[i] = essential[i] = false;

    for(int j=0 ; j<covered.size() ; ++j)
    {
        for(int i=0 ; i<row.size() ; ++i)
        {
            if(isCover(row[i].term, covered[j]))
                POS[j].push_back(i);
        }
        if(POS[j].size()==1)
            essential[POS[j].front()] = true;
    }

    for(int i=0 ; i<covered.size() ; ++i)
        valid[POS[i].front()] = true;
    int cnt = 0;
    for(int i=0 ; i<row.size() ; ++i)
        if(valid[i])
        {
            ans.push_back(row[i]);
            cnt += row[i].literalCount();
            valid[i] = false;
        }
    curAns = cnt;
    dfs(0,0);
}

void solve()
{
    if(n > THRESHOLD) // heuristic
    {
        Rearrange();
        Expand();
        Irredundant();
    }
    else // Q-M method
    {
        QM_method();
        PetrickMethod();
    }

}

void printAns()
{
    pdqsort(ans.begin(), ans.end());
    ans.erase(unique(ans.begin(),ans.end()),ans.end());
    int cnt = 0;
    for(pTerm p : ans)
        cnt += p.literalCount();
    fout << cnt << '\n';
    fout << ans.size() << '\n';
    for(pTerm p : ans)
        fout << p.term << '\n';
    fout.close();
}

int main(int argc, char* argv[])
{
    ios_base::sync_with_stdio(false);
    cin.tie(0);
    st = clock();
    fin.open(argv[1], ios::in);
    fout.open(argv[2], ios::out);
    init();
    solve();
    printAns();
    return 0;
}
