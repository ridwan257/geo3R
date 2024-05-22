IP = '192.168.40.46';
IP = '192.168.0.107';
IP = '127.0.0.1';


// --------------- Main Content ----------------------

const reader2 = new FileReader();
reader2.onload = (event) => {
	let contents = event.target.result;
	contents = contents.substring(contents.indexOf('\n'));
	query_genes = new Set(contents.split(/[\t\n]+/));
	query_genes.delete('');
	alert('Genes Loaded Successfully. ^_^');
}


document.getElementById('geneSubmit').addEventListener('click', ()=>{
	gene_file = document.getElementById('queryGene');
	
	if( gene_file.files.length == 0 ){
		alert('No Input!') ;
	} else {
		reader2.readAsText(gene_file.files[0]);
	}
});




// ------------------ handle empty table ---------------
document.addEventListener('DOMContentLoaded', () => {
  const tableContainer = document.querySelector('.table-container');
  const tableBody = tableContainer.querySelector('tbody');

  // Check if table body has no rows
  if (tableBody.rows.length === 0) {
    // Create a row with a single cell containing the empty text
    const row = tableBody.insertRow();
    const cell = row.insertCell();
    cell.colSpan = 2;
    cell.classList.add('empty-table');
    cell.textContent = 'Load GEO Data to View!';
    cell.classList.add('empty-text');
  }
});


// ---------------------- Load Custom Expression Data -----------------
function load_custom_data(){
	let eset = document.getElementById('exprs-data').files;
	let pdata = document.getElementById('meta-data').files;
	
	tsv_dump(eset[0])
	.then(data => {
		expression_data = data;

		return tsv_dump(pdata[0]);
	})
	.then(data => {
		metadata = data;
		
		load_data_into_table("pdata-viewer", metadata);
	})
	.catch( err => console.log(err) )



}



function analysis_selector(value){
	let custom = document.getElementById('custom-expression-set');
	let geo = document.getElementById("geo-handler");
	let title = document.querySelector('#metadata-container h3');

	if(value == 'geo'){
		custom.style = "display:none;";
		geo.style = "display:block";
		title.innerText = "Load GSE Data";
	}
	else if(value == 'custom'){
		custom.style = "display:block;";
		geo.style = "display:none";
		title.innerText = "Load Custom Data";
	}
}





// --------------------- Load Patients Data ---------------------
document.getElementById('geo-id').value = 'GSE149507';

function load_geo_data(){
	showLoadingScreen('Downloading Your Data for the first time. <br> It may take some time...');

	clear_all();

	gseID = document.getElementById('geo-id').value;
	
	return new Promise( res =>{
		
		fetch('http://'+IP+':8000/getGSE?id='+gseID)
		.then(response => response.json())
		.then(data => {

			let url = "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=" + gseID
			let p = document.querySelectorAll('#geo-handler p');
			p[0].innerHTML = `<b>Title :</b> <a href=${url} target="blank">${data.title[0]}</a>`;
			p[1].innerHTML = `<b>Data Processing :</b> ${data.data_processing[0]}`;

			// delete data.title;

			console.log(data)
			metadata = data.metadata;
			expression_data = data.edata;
			feature_data = data.fdata;

			load_data_into_table("pdata-viewer", data.metadata);
			hideLoadingScreen();

			res();

		})
		.catch(err => {
			console.log(err);
			hideLoadingScreen();
			alert(err);
		})
	

	});
}





// ----------------------- run differential expression ---------------------
function run_geo(){

	showLoadingScreen(); // render loading screen 

	fetch('http://'+IP+':8000/runDEG', {
		method : 'POST',
		headers : {
			'Content-Type' : 'application/json'
		},
		body : JSON.stringify(
		{
			id : gseID,
			edata : expression_data,
			metadata : metadata,
			fdata : feature_data,
			// control : localStorage.getItem('con').split(','),
			// case : localStorage.getItem('cas').split(',')
			control : Array.from(control_group),
			case : Array.from(case_group)
		})
	})
	.then( response => response.json() )
	.then(data => {

		console.log('Loaded Data Successfully...!');
		console.log(data);

		// plot hclust
		document.getElementById('hclust-main').src = 'data:imgae/png;base64,' + data.dendogram[0];

		// plot PCA
		document.querySelector('#pca-plot h5').innerText = `PCA variance explained by 2 axis = ${data.pca.varE[0]} %`;
		document.getElementById('pca-var').src = 'data:imgae/png;base64,' + data.pca.var_plot[0];
		document.getElementById('pca-main').src = 'data:imgae/png;base64,' + data.pca.plot[0];

		// plot heatmap
		document.getElementById('heatmap-main').src = 'data:imgae/png;base64,' + data.heatmap[0];

		// plot volcano
		document.getElementById('volcano-main').src = 'data:imgae/png;base64,' + data.volcano[0];
		default_volcano_img = data.volcano[0];

		dfe_results = data.deg;

		hideLoadingScreen(); // hide loading screen

	})
	.catch(err => {
		console.log(err);
		hideLoadingScreen();
		alert(err);
	})
}

// ----------------- show differentially expressed gene ------------------
document.querySelector("#view-container a").addEventListener('click', ()=>{
	// console.log('hi');
	localStorage.setItem('dfe_results', JSON.stringify(dfe_results));
});


// -------------------- Customize Volcano Plot ------------------------------
// show default volacno plot
document.getElementById('defult-plot').addEventListener('click', ()=>{
	document.getElementById('volcano-main').src = 'data:imgae/png;base64,' + default_volcano_img;
});

// mark input genes
function mark_input_genes(){
	// let gene_names = ["SLC26A4", "ALDH3A1", "DUSP1", "FOS", "ADH7", "AKR1B10", "HBB", "UCHL1",
		// "SLC7A11", "NQO1", "TMEM45A", "CYP1B1", "GPX2"];

	let alpha = Number(document.getElementById('alpha-value').value);
	let test_stat_name = document.getElementById('p-value').value;
	let logFC = [];
	Array.from(document.getElementsByClassName('logFC')).forEach( el => logFC.push(Number(el.value)) );


	load_gene_names()
	.then(gene_names =>{
		gene_names = Array.from(gene_names);
		if ( gene_names.length == 0 ) {
			alert('No Input Genes!')
		} else {

			showLoadingScreen();

			fetch('http://'+IP+':8000/volcano/mark_gene', {
				method : 'POST',
				headers : {
					'Content-Type' : 'application/json'
				},
				body : JSON.stringify(
				{
					'results' : dfe_results,
					'minFC' : logFC[0],
					'maxFC' : logFC[1],
					'genes' : gene_names,
					'alpha' : alpha,
					'by' : test_stat_name
				})
			})
			.then(response => response.json())
			.then( data => {
				document.getElementById('volcano-main').src = 'data:imgae/png;base64,' + data.volcano[0];
				
				hideLoadingScreen();

				if(data.not_found.length > 0){
					alert(`${data.not_found} are not in results.`);
				}

			})
			.catch(err =>{
				console.log(err);
				hideLoadingScreen();
				alert(err);
			})
		}
	});	

}

// --------------- volcano plot with custom alpha and logFC --------------
function reload_volcano_plot(){
	let alpha = Number(document.getElementById('alpha-value').value);
	let test_stat_name = document.getElementById('p-value').value;
	let logFC = [];
	Array.from(document.getElementsByClassName('logFC')).forEach( el => logFC.push(Number(el.value)) );


	showLoadingScreen();

	fetch('http://'+IP+':8000/volcano', {
		method : 'POST',
		headers : {
			'Content-Type' : 'application/json'
		},
		body : JSON.stringify(
		{
			'results' : dfe_results,
			'minFC' : logFC[0],
			'maxFC' : logFC[1],
			'alpha' : alpha,
			'by' : test_stat_name
		})
	})
	.then(res => res.json())
	.then(data => {
		document.getElementById('volcano-main').src = 'data:imgae/png;base64,' + data.volcano[0];

		hideLoadingScreen();
	})
	.catch(err=>{
		hideLoadingScreen();
		alert(err);
	})
}




// ----------------- copy gene list ------------------
function copy_gene_list(){
	return new Promise((res, rej) => {
		let gene_container = document.querySelectorAll("#gene-list div p");
		let gene_names = '';
		gene_container.forEach(p => { gene_names += p.innerText + '\n'; });
		
		load_to_clipboard(gene_names)
		.then( ()=> res() )
		.catch(err => rej(err));
	});

}

// -------------- copy button functions in pop-up box ----------------
document.querySelector('.copy').addEventListener('click', ()=>{
	copy_gene_list()
	.then( () => document.getElementById('gene-list').style = 'display:none;' )
	.catch( err => console.log(err) );
});



function load_ppi(){
	let gene = get_deg_gene();

	if (gene.length!=0){
		// let url = "https://string-db.org/api/image/network?identifiers=" + gene.names.join('%0d') + "&species=9606";
		
		showLoadingScreen();

		let gene_names = [];
		gene.forEach( x => gene_names.push(x.name) )

		console.log(gene_names);

		getSTRING('https://string-db.org', {
      'species':'9606',
      'identifiers': gene_names,
      'network_flavor':'confidence'
    });
    
    hideLoadingScreen();

	}
	else {
		console.log('there is no gene maggi');
	}

}























