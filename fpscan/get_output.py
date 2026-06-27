import yaml
import re

with open("o.out", "r") as fd:
    raw_in = fd.read()

def remove_e_expression(expr):
    if ("e" in expr):
        l = expr.split("e")
        m = float(l[0])
        e = float(l[1])
        r = m * 10**e
        print(r)
        return f"{r:f}"
    else:
        return expr

raw_in = re.sub(r"\{[^ ]*\n", "{\n", raw_in)
raw_in = raw_in.replace("\n", "")
is_in = False
i = 0
raw_lines = []
buf = ""

while i <= len(raw_in) - 2:
    if raw_in[i:i+2] == "/*":
        is_in = True
        i += 2
    elif raw_in[i:i+2] == "*/":
        is_in = False
        raw_lines.append(buf)
        buf = ""
        i += 2
    elif is_in:
        buf += raw_in[i]
        i += 1
    else:
        i += 1

last_line: str = raw_lines[-1]
last_line = re.sub(r"interval[^ ]*", "", last_line).replace(" ", "")

print(last_line)

def parse_dict_h(dict_str, i):
    def get_key(s, i):
        buff = None
        while (i <= len(s) - 1):
            c = s[i]
            match c:
                case ":":
                    # end of key def
                    if buff is None:
                        raise Exception("No key name before ':'")
                    return buff, i+1
                case _:
                    if buff is None:
                        buff = c
                    else:
                        buff += c
            i += 1
    
    def get_value(s, i):
        def get_interval(s, i):
            c = s[i]
            if c != "[":
                # not an interval
                return None, i
            
            # skip [
            i += 1
            
            # find enclosing ]
            buff = ""
            l = None
            u = None
            while (i <= len(s) - 1):
                c = s[i]
                match c:
                    case "]":
                        # end of match
                        # lower bound b$must be set
                        if l is None:
                            raise Exception("missing lower bound")
                        u = buff
                        return [l, u], i+1
                    case ",":
                        # matched lower bound
                        if l is not None:
                            raise Exception("not many coma in interval")
                        l = buff
                        buff = ""
                    case " ":
                        # skip space
                        pass
                    case _:
                        buff += c
                i += 1
            raise Exception("interval match failed")
        match s[i]:
            case "[":
                return get_interval(s, i)
            case "{":
                # find enclosing }
                brace_lvl = 1
                end_index = i + 1
                while (end_index <= len(s) - 1):
                    match s[end_index]:
                        case "{":
                            brace_lvl += 1
                        case "}":
                            brace_lvl -= 1
                    if brace_lvl == 0:        
                        return parse_dict_h(s[i:end_index+1], i)
                    end_index+=1
            case _:
                raise Exception("invalid value")

    entry_dict = {}

    # remove enclosing "{}"
    dict_str = dict_str[1:-1]

    i = 0
    key = None
    buf = None
    while (i <= len(dict_str) - 1):
        match key:
            case None:
                key, i = get_key(dict_str, i)
            case _:
                value, i = get_value(dict_str, i)
                # skip ","
                i += 1

                entry_dict[key] = value
                key = None
    return entry_dict
    

print("\n")

entry_dict = parse_dict_h(last_line, 0)
print(entry_dict)

with open("o.yaml", "w") as fd:
    yaml.dump(entry_dict, fd)