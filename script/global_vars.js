
SESSION_ID = `r_${Math.random().toString(36).slice(2, 6)}_${Date.now()}`;
let R_URL = "http://127.0.0.1:8080"

let GSE_ID = "";
let PDATA = [];
let FDATA = [];
let DEG_RESULT = [];

let CASE_GROUPS = new Set();
let CONTROL_GROUPS = new Set();

// CONTROL_GROUPS = new Set(["GSM927630", "GSM927631", "GSM927632"]);
// CASE_GROUPS = new Set(["GSM927639", "GSM927640", "GSM927641"]);

CONTROL_GROUPS = new Set(["GSM5411286", "GSM5411287", "GSM5411288", "GSM5411289", "GSM5411290", "GSM5411291", "GSM5411292", "GSM5411293", "GSM5411294"])
CASE_GROUPS = new Set(["GSM5411277", "GSM5411278", "GSM5411279", "GSM5411280", "GSM5411281", "GSM5411282", "GSM5411283", "GSM5411284", "GSM5411285"])


// ----- varialbe to control page table for deg genes
let CURRENT_PAGE = 1;
let ENTRY_PER_PAGE = 50;
let CURRENTLY_SORTED = { key: null, asc: true };
let FILTERED_DATA = [];

