

document.getElementById('load-geo-btn').addEventListener('click', async()=>{
	disable_buttons();
	RESET_ALL();
	try{
		GSE_ID = document.getElementById('geo-id').value;

		const res = await fetch(`${R_URL}/load_geo`, {
			method : 'POST',
			headers: {
				'Content-Type': 'application/json'
			},
			body: JSON.stringify({
				session_id: SESSION_ID,
				gse_id: GSE_ID
			})
		});

		const data = await res.json();

		if(!data.data_found[0]){
			alert("Cannot download data. Make sure of stable internet connection or correct GSE ID.")
			return;
		}
		if(!data.total_platform[0] > 1){
			alert("Multiple platform found for data. Seleting only the first one!")
		}

		console.log(data);

		document.getElementById('info-geo-id').innerHTML = GSE_ID;
		document.getElementById("info-title").innerHTML = data.title;
		document.getElementById("info-annotation").innerHTML = data.annotation;
		document.getElementById("info-samples").innerHTML = data.n_samples;
		document.getElementById("info-data-processing").innerHTML = data.data_processing;

		PDATA = data.metadata;
		FDATA = data.fdata;


		// metadata loading
		load_data_into_table( "pdata-viewer", PDATA, () => set_columns('column_dropdown', 'pdata-viewer'),
			(key, tr, row) => {
				// load previous history
				if(key == 'PID'){
					if(CASE_GROUPS.has(row[key])){
						tr.classList.add('case');
					} else if (CONTROL_GROUPS.has(row[key])){
						tr.classList.add('control');
					}
				}
			}
		);

		initilize_dropdown_columns('column_dropdown', 'pdata-viewer', false);

		const checkbox_arr = Array.from(document.querySelectorAll("#column_dropdown label input"));
		
		Array.from(document.querySelectorAll("#pdata-viewer thead th")).forEach((th, i) => {
			let colname = th.innerText;
			if( colname == 'PID' || colname == 'title' || colname.includes(':') ) checkbox_arr[i].checked = true;
		});
		set_columns('column_dropdown', 'pdata-viewer');
		
		// feature data loding
		load_data_into_table( "fdata-viewer", FDATA, () => set_columns('column_dropdown2', 'fdata-viewer') );
		initilize_dropdown_columns('column_dropdown2', 'fdata-viewer', true);

	}
	catch(error){
		console.log(error)
	}
	finally{
		enable_buttons();
	}
});

// ====================================================================================
// ====================================================================================
document.getElementById('run_analysis_btn').addEventListener('click', async () => {
	if (CASE_GROUPS.size < 1 && CONTROL_GROUPS.size < 1) {
		alert('Case and control sample must be >1.');
		return;
	}

	try {
		disable_buttons();

		const res = await fetch(`${R_URL}/run_analysis`, {
			method: 'POST',
			headers: { 'Content-Type': 'application/json' },
			body: JSON.stringify({
				session_id: SESSION_ID,
				analysis_options: get_analysis_options(),
				sample_list: get_selected_samples()
			})
		});

		if (!res.ok) {
			const err_text = await res.text();
			throw new Error(`Error ${res.status} : ${err_text}`);
		}

		const data = await res.json();

		// load plots
		const time_now = Date.now();
		document.getElementById('boxplot-edata-img').src = `./tmp/${SESSION_ID}_dist1.png?cb=${time_now}`;
		document.getElementById('densityplot-edata-img').src = `./tmp/${SESSION_ID}_dist2.png?cb=${time_now}`;
		document.getElementById('pca-edata-img').src = `./tmp/${SESSION_ID}_pca1.png?cb=${time_now}`;
		document.getElementById('hclust-edata-img').src = `./tmp/${SESSION_ID}_hclust1.png?cb=${time_now}`;
		document.getElementById('volcano-edata-img').src = `./tmp/${SESSION_ID}_volcano1.png?cb=${time_now}`;

		DEG_RESULT = data.deg_table;
		CURRENT_PAGE = 1;
		FILTERED_DATA = DEG_RESULT;
		render_data_to_page_table();

		update_groupby_from_pdata("group-option-pca");
		update_groupby_from_pdata("group-option-hclust");

		console.log("Analysis Done...");

	} catch (error) {
		console.error("Analysis failed:", error);
		alert(`Analysis failed: ${error.message}`);
	} finally {
		enable_buttons();
	}
});

// ====================================================================================
// ====================================================================================
document.getElementById("group-option-pca-btn").addEventListener("click", async ()=>{
	const res = await fetch(`${R_URL}/update_pca_plot`, {
		method : 'POST',
		headers: {
			'Content-Type': 'application/json'
		},
		body: JSON.stringify({
			session_id: SESSION_ID,
			group_by : document.getElementById("group-option-pca").value,
			legend_pos : document.getElementById("legend-option-pca").value
		})
	});

	if (!res.ok) {
		const err_text = await res.text();
		throw new Error(`Error ${res.status} : ${err_text}`);
	}

	const time_now = Date.now();
	document.getElementById('pca-edata-img').src = `./tmp/${SESSION_ID}_pca1.png?cb=${time_now}`;

});

// ====================================================================================
// ====================================================================================
document.getElementById("group-option-hclust-btn").addEventListener("click", async ()=>{
	const res = await fetch(`${R_URL}/update_hclust_plot`, {
		method : 'POST',
		headers: {
			'Content-Type': 'application/json'
		},
		body: JSON.stringify({
			session_id: SESSION_ID,
			group_by : document.getElementById("group-option-hclust").value,
			legend_pos : document.getElementById("legend-option-hclust").value
		})
	});

	if (!res.ok) {
		const err_text = await res.text();
		throw new Error(`Error ${res.status} : ${err_text}`);
	}

	const time_now = Date.now();
	document.getElementById('hclust-edata-img').src = `./tmp/${SESSION_ID}_hclust1.png?cb=${time_now}`;

});

// ====================================================================================
// ====================================================================================
async function update_volcano_plot() {
	const res = await fetch(`${R_URL}/update_volcano_plot`, {
		method : 'POST',
		headers: {
			'Content-Type': 'application/json'
		},
		body: JSON.stringify({
			session_id: SESSION_ID,
			plot_params : get_volcano_plot_options()
		})
	});

	if (!res.ok) {
		const err_text = await res.text();
		throw new Error(`Error ${res.status} : ${err_text}`);
	}

	console.log('update volcano');
	
	const time_now = Date.now();
	document.getElementById('volcano-edata-img').src = `./tmp/${SESSION_ID}_volcano1.png?cb=${time_now}`;
}

// ====================================================================================
// ====================================================================================
document.getElementById('gene-boxplot-btn').addEventListener('click', async () => {

	const gene_names = document.getElementById("gene-names-boxplot").value
                .split(/[\t\s\n,]+/)
                .map(str => str.trim())
                .filter(str => str.length > 0);

	if(gene_names.length === 0) {
		alert('No Gene.')
	}

	const res = await fetch(`${R_URL}/strip_plot_gene`, {
		method : 'POST',
		headers: {
			'Content-Type': 'application/json'
		},
		body: JSON.stringify({
			session_id: SESSION_ID,
			gene_names : gene_names,
			n_col : parseInt(document.getElementById('n-col-boxplot').value) || 4
		})
	});

	if (!res.ok) {
		const err_text = await res.text();
		throw new Error(`Error ${res.status} : ${err_text}`);
	}

	const data = await res.json();

	if(data.had_plot[0]){
		const time_now = Date.now();
		document.getElementById('gene-boxplot-img').src = `./tmp/${SESSION_ID}_strip_plot1.png?cb=${time_now}`;
	}
	else{
		alert("No genes were found!")
	}
	
});

// ====================================================================================
// ====================================================================================
window.addEventListener("beforeunload", async () => {

    await fetch(`${R_URL}/kill_session`, {
        method:"POST", 
        keepalive:true,
        headers: {
			'Content-Type': 'application/json'
		},
		body: JSON.stringify({
			session_id: SESSION_ID
		})
    })

});