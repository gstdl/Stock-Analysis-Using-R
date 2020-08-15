import pandas as pd
import pandas_datareader as pdr
from tqdm import tqdm


tickers = pd.read_excel('data_input/Yahoo Ticker Symbols - September 2017.xlsx').iloc[3:, :2]
output_file = 'shinyapp/data_input/tickers.csv'


try:
    f = pd.read_csv(output_file, sep = ';')
    file_exist = True
    tickers = tickers[~tickers.iloc[:, 0].isin(f['ticker'])]
except:
    f = pd.DataFrame(None, None, ['ticker', 'name'])
    file_exist = False
    

data_added = len(f)
start_date = '2020-08-03'
end_date = '2020-08-07'
new_tickers = []


def query_func(ticker, name):
    if ticker in f['ticker']:
        return 0
    else:
        try:
            pdr.DataReader(ticker, 'yahoo', start = start_date, end = end_date)
            new_tickers.append([ticker, name])
            return 1
        except:
            return 0


progress_bar = tqdm(enumerate(tickers.index))


for i, idx in progress_bar:
    ticker, name  = tickers.loc[idx]
    progress_bar.set_description_str(f'data added = {data_added}', refresh = False)
    progress_bar.set_postfix_str(f'working on : {ticker}, {name}')    
    data_added += query_func(ticker, name)
    if i % 5000 == 0:
        f = f.append(pd.DataFrame(new_tickers, columns = f.columns), ignore_index = True)
        f.to_csv(output_file, index = False, sep = ';')
        new_tickers = []
else:
    progress_bar.set_description_str(f'data added = {data_added}', refresh = True)
    f = f.append(pd.DataFrame(new_tickers, columns = f.columns), ignore_index = True)
    f.to_csv(output_file, index = False, sep = ';')
    print('CLEANING COMPLETED!')