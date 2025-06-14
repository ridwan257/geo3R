
const volcano_checkbox_names = ["mark-fc", "highlight-check"];
for(let i=0; i<volcano_checkbox_names.length; i++){
		document.getElementById(volcano_checkbox_names[i]).addEventListener("change", event => {
			if (event.target.checked) {
				for(let j=0; j < volcano_checkbox_names.length; j++){
					if(i != j) { document.getElementById(volcano_checkbox_names[j]).checked = false; }
				}	
			}
		});
}



// ====================================================================================
// ====================================================================================
function openTab(tabName) {
	const tabs = document.getElementsByClassName("tab-content-box");
	for (let i = 0; i < tabs.length; i++) {
		tabs[i].style.display = "none";
	}
	document.getElementById(tabName).style.display = "block";
}

// ====================================================================================
// ====================================================================================
function toggleCollapseOptions() {
	const method = document.getElementById("collapse_method_selector").value;

	document.getElementById("gene_collapse_options").style.display = method === "gene" ? "block" : "none";
	document.getElementById("gene_mapping_column").style.display = method === "gene" ? "block" : "none";
	document.getElementById("gene_symbol_transform").style.display = method === "gene" ? "block" : "none";
		
	if (FDATA && method == "gene"){
		const select_opt = document.getElementById("mapping-col");
		select_opt.innerHTML = "";

		Object.keys(FDATA[0]).forEach(colname => {
			if(colname != "ID"){
				const opt = document.createElement("option");
				opt.value = colname;
				opt.textContent = colname;
				select_opt.appendChild(opt);
			}
		});
	}

}

// ====================================================================================
// ====================================================================================
function toggleFilterN() {
	const checked = document.getElementById("filter_genes").checked;
	document.getElementById("filter_n_input").style.display = checked ? "block" : "none";
}

// ====================================================================================
// ====================================================================================
function get_analysis_options(){
	return {
		normalize : document.getElementById("quantile-normalize").checked,
		log_transform : document.getElementById("log-trnsform").checked,
		collapse_to_gene : document.getElementById("collapse_method_selector").value,
		collapse_method: document.getElementById('collapse_method').value,
		mapping_column: document.getElementById('mapping-col').value,
		convert_from_entrez : document.getElementById('convert-from-entrez').checked,
		filter_gene: document.getElementById('filter_genes').checked,
		filter_n: document.getElementById('filter_n').value
	};
}

function reset_analysis_options(){
	document.getElementById("quantile-normalize").checked = false;
	document.getElementById("log-trnsform").checked = false;
	document.getElementById("collapse_method_selector").value = 'probe';
	document.getElementById('convert-from-entrez').checked = false;
	document.getElementById('filter_genes').checked = false;
	document.getElementById('filter_n').value = 2;
}

// ====================================================================================
// ====================================================================================
function get_selected_samples(){
	return {
		control : Array.from(CONTROL_GROUPS),
		case : Array.from(CASE_GROUPS)
	}
}

// ====================================================================
// ------------- volcano plot related
// ====================================================================
function reset_volcano_options(){
    document.getElementById("mark-input").checked = false;
    document.getElementById("mark-fc").checked = false;
    document.getElementById("highlight-check").checked = true;
    document.getElementById("highlight-value").value = 8;
    document.getElementById("highlight-by").value = "Sig";
    document.getElementById("min-fc").value = -1;
    document.getElementById("max-fc").value = 1;
    document.getElementById("alpha-val").value = 0.05;
    document.getElementById("y-axis").value = "P.Value";
}

function get_volcano_plot_options(){

    const gene_names = document.getElementById("gene-names").value
                .split(/[\t\s\n,]+/)
                .map(str => str.trim())
                .filter(str => str.length > 0);
                // .map(str => str.toUpperCase());

    return {
        mark_genes : document.getElementById("mark-input").checked,
        gene_names : gene_names,
        mark_sig_genes : document.getElementById("mark-fc").checked,
        highlight : document.getElementById("highlight-check").checked,
        highlight_n : document.getElementById("highlight-value").value,
        highlight_by : document.getElementById("highlight-by").value,
        min_fc : document.getElementById("min-fc").value,
        max_fc : document.getElementById("max-fc").value,
        alpha_val : document.getElementById("alpha-val").value,
        pval_by : document.getElementById("y-axis").value,
    };
}
// ====================================================================================
// ====================================================================================
// DEG data logic
async function get_deg_genes() {
  	const min_fc = parseFloat(document.getElementById("min-fc").value);
	const max_fc = parseFloat(document.getElementById("max-fc").value);
    const alpha_val = parseFloat(document.getElementById("alpha-val").value);
	const pval_by = document.getElementById("y-axis").value;
	
	const deg_genes = [];
	const chunk_size = 500;
	let i = 0;

	return new Promise( (resolve, reject) => {
		function processChunk() {

			if(DEG_RESULT.length == 0){ reject("Run analysis to see DEG.") }

			const end = Math.min(i + chunk_size, DEG_RESULT.length);

			// the small chunk processing
			for (; i < end; i++) {
				const row = DEG_RESULT[i];
				if ((row.logFC >= max_fc || row.logFC <= min_fc) && row[pval_by] <= alpha_val) {
					deg_genes.push(row.Gene);
				}
			}

			if (i < DEG_RESULT.length) {
				setTimeout(processChunk, 0);
			} else {
				resolve(deg_genes);
			}
		}
		processChunk();
	} )
}

function close_deg_modal() {
  document.getElementById('degModal').style.display = 'none';
}

document.getElementById('show-deg-btn').addEventListener('click', async () =>{
	
	const modal = document.getElementById('degModal');
	modal.style.display = 'block';

	const textarea = document.getElementById('deg-gene-list');
	
	try{
		const gene_names = await get_deg_genes();
		textarea.value = gene_names.join('\n');
	}
	catch(error){
		textarea.value = `ERROR: ${error}`;
	}
	
});



function copy_deg_genes() {
  const textarea = document.getElementById('deg-gene-list');
  textarea.select();
  textarea.setSelectionRange(0, 99999);

  navigator.clipboard.writeText(textarea.value)
    .then(() => {
      alert("Copied to clipboard");
    })
    .catch(err => {
      console.error("Copy failed", err);
    });
}

// ====================================================================================
// ----- PAGE TABLE for DEG viewer
// ====================================================================================
// toy data
// DEG_RESULT = Array.from({length: 513}, (_, i) => ({
// 	Gene: "Gene" + (i+1),
// 	logFC: (Math.random() * 4 - 2).toFixed(2),
// 	pval: Math.random().toFixed(4),
// 	"adj.P.Val": Math.random().toFixed(4)
// }));

function render_data_to_page_table() {
	const start_idx = (CURRENT_PAGE - 1) * ENTRY_PER_PAGE;
	const end_idx = Math.min(start_idx + ENTRY_PER_PAGE, FILTERED_DATA.length);
	const tbody = document.querySelector("#deg-viewer tbody");
	const thead = document.querySelector("#deg-viewer thead");

	tbody.innerHTML = "";
	thead.innerHTML = "";

	if (FILTERED_DATA.length == 0){
		document.querySelectorAll(".page-info").forEach(el => {
			el.textContent = `Page ${CURRENT_PAGE} of ${Math.ceil(FILTERED_DATA.length / ENTRY_PER_PAGE)}`;
		});
		return;
	}

	const colanames_arr = Object.keys(FILTERED_DATA[0]);

	colanames_arr.forEach(col => {
		const th = document.createElement("th");
		th.textContent = col;
		th.dataset.sort = col;
		th.style.cursor = "pointer";
		th.onclick = () => sort_page_table(col);
		thead.appendChild(th);
	});

	// Body
	for (let i = start_idx; i < end_idx; i++) {
		const row = FILTERED_DATA[i];
		const tr = document.createElement("tr");
		colanames_arr.forEach(col => {
			const td = document.createElement("td");
			td.textContent = row[col];
			tr.appendChild(td);
		});
		tbody.appendChild(tr);
	}

	document.querySelectorAll(".page-info").forEach(el => {
		el.textContent = `Page ${CURRENT_PAGE} of ${Math.ceil(FILTERED_DATA.length / ENTRY_PER_PAGE)}`;
	})
	
}

async function search_page_table() {
	const search = document.getElementById("search-input").value.toLowerCase();

	let i = 0;
	const chunk_size = 500;
	const result = [];

	return new Promise((resolve, reject) => {
		if (!DEG_RESULT || DEG_RESULT.length === 0) {
			reject("Run Analysis to get DEG results...");
			return;
		}

		function __search() {
			const end_idx = Math.min(i + chunk_size, DEG_RESULT.length);

			for (; i < end_idx; i++) {
				const gene = DEG_RESULT[i]["Gene"];
				if (gene && gene.toLowerCase().includes(search)) {
					result.push(DEG_RESULT[i]);
				}
			}

			if (i < DEG_RESULT.length) {
				setTimeout(__search, 0);
			} else {
				resolve(result);
			}
		}
		__search();
	});
}

function sort_page_table(key) {
	if(FILTERED_DATA.length == 0) return;

	CURRENTLY_SORTED.asc = CURRENTLY_SORTED.key === key ? !CURRENTLY_SORTED.asc : true;
	CURRENTLY_SORTED.key = key;
	FILTERED_DATA.sort((a, b) => {
		const v1 = parseFloat(a[key]) || a[key];
		const v2 = parseFloat(b[key]) || b[key];
		return CURRENTLY_SORTED.asc ? (v1 > v2 ? 1 : -1) : (v1 < v2 ? 1 : -1);
	});
	CURRENT_PAGE = 1;
	render_data_to_page_table();
}

function prev_page_table() {
	if (FILTERED_DATA.length !== 0 && CURRENT_PAGE > 1) {
		CURRENT_PAGE--;
		render_data_to_page_table();
	}
}

function next_page_table() {
	if (FILTERED_DATA.length !== 0 && CURRENT_PAGE < Math.ceil(FILTERED_DATA.length / ENTRY_PER_PAGE)) {
		CURRENT_PAGE++;
		render_data_to_page_table();
	}
}

function goto_page_table(input_id){
	const value = parseInt(document.getElementById(input_id).value) || -1;
	if(value > 0 && value <= Math.ceil(FILTERED_DATA.length / ENTRY_PER_PAGE)){
		CURRENT_PAGE = value;
		render_data_to_page_table();
		return;
	}

	alert("Enter valid page no...")
}

function download_page_table() {
	if(DEG_RESULT.length === 0) {
		alert("No data to export!");
		return;
	}

	// Get column headers
	const headers = Object.keys(DEG_RESULT[0]);
	let tsv_content = headers.join("\t") + "\n";

	// Add each row
	DEG_RESULT.forEach(row => {
		const line = headers.map(col => String(row[col] ?? "")).join("\t");
		tsv_content += line + "\n";
	});

	// Create blob and trigger download
	const blob = new Blob([tsv_content], { type: "text/tab-separated-values" });
	const url = URL.createObjectURL(blob);

	const a = document.createElement("a");
	a.href = url;
	a.download = `top_table_${GSE_ID}.tsv`;
	document.body.appendChild(a);
	a.click();
	document.body.removeChild(a);
	URL.revokeObjectURL(url);
}

// ---- search box
document.getElementById("search-input").addEventListener("input", async () => {

	try{
		FILTERED_DATA = await search_page_table();
		CURRENT_PAGE = 1;
		render_data_to_page_table();
	}
	catch(error) {
		console.log(`ERROR: ${error}`);
	}

});
// ------- select total entries per page
document.getElementById("entries-select").addEventListener("change", e => {	
	ENTRY_PER_PAGE = parseInt(e.target.value);
	CURRENT_PAGE = 1;
	render_data_to_page_table();
});